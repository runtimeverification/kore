{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module SQL (
    -- * Column
    Column (..),
    defineTextColumn,
    mkColumnImpl,
    mkColumnImpls,
    module SQL.ColumnDef,

    -- * Table
    defineForeignKeyColumn,
    toForeignKeyColumn,
    TableName (..),
    Table (..),
    insertUniqueRow,

    -- * Generic table types
    createTableGeneric,
    insertRowGeneric,
    selectRowGeneric,

    -- * Table isomorphisms
    createTableIso,
    insertRowIso,
    selectRowIso,

    -- * Table newtypes
    createTableUnwrapped,
    insertRowUnwrapped,
    selectRowUnwrapped,

    -- * Re-exports
    Proxy (..),
    module SQL.Key,
    module SQL.SQL,
) where

import Control.Lens qualified as Lens
import Control.Monad.Extra qualified as Monad
import Data.Generics.Wrapped
import Data.Int (
    Int64,
 )
import Data.Proxy (
    Proxy (..),
 )
import Data.Text (
    Text,
 )
import Database.SQLite.Simple qualified as SQLite
import Generics.SOP qualified as SOP
import Prelude.Kore
import SMT.SimpleSMT qualified as SMT (
    Result (..),
 )
import SQL.ColumnDef
import SQL.Key
import SQL.SOP (
    ColumnImpl (..),
    TableName (..),
 )
import SQL.SOP qualified as SOP
import SQL.SQL

-- * Column

class Column a where
    -- | Return a 'ColumnDef' suitable for defining the column in a table.
    --
    --    If the column is a foreign key reference, the referenced table will be
    --    defined if it does not exist. The 'TableName' will be used to detect
    --    possible cycles when defining tables for recursive datatypes. The
    --    'TableName' should be the name of the table for the type; for sum types, it
    --    should be the name of the type table, not the name of a constructor table,
    --    because the constructor tables are only referenced by the type table and all
    --    foreign keys are references to the type table.
    defineColumn :: TableName -> proxy a -> SQL ColumnDef

    toColumn :: a -> SQL SQLite.SQLData

-- TODO (thomas.tuegel): Implement this!
-- fromColumn :: SQLite.Connection -> SQLite.SQLData -> IO a

instance Column Int64 where
    defineColumn _ _ = return (columnNotNull $ columnDef typeInteger)
    toColumn = return . SQLite.SQLInteger

instance Column (Key a) where
    defineColumn tableName _ = defineColumn tableName (Proxy @Int64)
    toColumn = toColumn . getKey

instance Column Text where
    defineColumn _ _ = return (columnNotNull $ columnDef typeText)
    toColumn = return . SQLite.SQLText

instance (Column a, Typeable a) => Column [a] where
    defineColumn = defineForeignKeyColumn
    {-# INLINE defineColumn #-}

    toColumn = toForeignKeyColumn
    {-# INLINE toColumn #-}

instance (Column a, Typeable a) => Column (Maybe a) where
    defineColumn = defineForeignKeyColumn
    {-# INLINE defineColumn #-}

    toColumn = toForeignKeyColumn
    {-# INLINE toColumn #-}

instance (Column a, Typeable a) => Column (NonEmpty a) where
    defineColumn = defineForeignKeyColumn
    {-# INLINE defineColumn #-}

    toColumn = toForeignKeyColumn
    {-# INLINE toColumn #-}

defineTextColumn :: TableName -> proxy a -> SQL ColumnDef
defineTextColumn tableName _ = defineColumn tableName (Proxy @Text)

instance Column SMT.Result where
    defineColumn = defineTextColumn

    toColumn SMT.Unsat = toColumn @Text "unsat"
    toColumn SMT.Sat = toColumn @Text "sat"
    toColumn SMT.Unknown = toColumn @Text "unknown"

-- | Reify a 'Column' constraint into a concrete implementation 'ColumnImpl'.
mkColumnImpl :: forall a. Column a => ColumnImpl a
mkColumnImpl =
    ColumnImpl
        { defineColumnImpl = defineColumn
        , toColumnImpl = toColumn
        }

{- | Implement 'defineColumn' for a foreign key reference.

The referenced table will be created if it does not exist.
-}
defineForeignKeyColumn :: Table a => TableName -> proxy a -> SQL ColumnDef
defineForeignKeyColumn tableName proxy = do
    unless (tableName == tableNameOf proxy) (createTable proxy)
    defineColumn tableName (Proxy @Int64)

{- | Implement 'toColumn' for a foreign key reference.

Inserts the given data into the table and returns a key to the inserted row.
-}
toForeignKeyColumn :: Table table => table -> SQL SQLite.SQLData
toForeignKeyColumn a = insertUniqueRow a >>= toColumn

{- | Reify all 'Column' constraints of a datatype.

See also: 'mkColumnImpl'
-}
mkColumnImpls :: forall xss. SOP.All2 Column xss => SOP.POP ColumnImpl xss
mkColumnImpls = SOP.hcpure (Proxy @Column) mkColumnImpl

-- * Table

{- | A 'Table' corresponds to a table in SQL.

To derive an instance for your type,

@
-- Note: All fields must have a 'Column' instance.
data DataType = ...
    deriving stock ('GHC.Generics.Generic', 'Data.Typeable.Typeable')
    deriving anyclass (Binary)

instance 'Generics.SOP.Generic' DataType

instance 'Generics.SOP.HasDatatypeInfo' DataType

instance Table DataType

-- Recommended: Add a foreign key 'Column' instance if other tables
-- might refer to DataType.
instance 'Column' DataType where
    defineColumn = 'defineForeignKeyColumn'
    toColumn = 'toForeignKeyColumn'
@
-}
class Typeable a => Table a where
    tableNameOf :: proxy a -> TableName
    default tableNameOf :: proxy a -> TableName
    tableNameOf = SOP.tableNameTypeable

    -- | Create the table for @a@ if it does not exist.
    createTable :: proxy a -> SQL ()
    default createTable ::
        SOP.HasDatatypeInfo a =>
        SOP.All2 Column (SOP.Code a) =>
        proxy a ->
        SQL ()
    createTable = createTableGeneric

    -- | Insert the @a@ as a new row in the table.
    --
    --    Returns the 'Key' of the inserted row.
    insertRow :: a -> SQL (Key a)
    default insertRow ::
        SOP.HasDatatypeInfo a =>
        SOP.All2 Column (SOP.Code a) =>
        a ->
        SQL (Key a)
    insertRow = insertRowGeneric

    -- | Find the 'Key' for an @a@, if it is in the table.
    selectRow :: a -> SQL (Maybe (Key a))
    default selectRow ::
        SOP.HasDatatypeInfo a =>
        SOP.All2 Column (SOP.Code a) =>
        a ->
        SQL (Maybe (Key a))
    selectRow = selectRowGeneric

instance Table ()

instance (Column a, Typeable a) => Table (Maybe a)

instance (Column a, Typeable a, Column b, Typeable b) => Table (Either a b)

instance (Column a, Typeable a) => Table [a]

instance (Column a, Typeable a) => Table (NonEmpty a)

{- | @(insertUniqueRow a)@ inserts @a@ into the table if not present.

Returns the 'Key' of the row corresponding to @a@.
-}
insertUniqueRow :: Table a => a -> SQL (Key a)
insertUniqueRow a = Monad.maybeM (insertRow a) return (selectRow a)

-- * Generic table types

-- | @createTableGeneric@ implements 'createTable' for a 'SOP.Generic' type.
createTableGeneric ::
    forall proxy table.
    Table table =>
    (SOP.HasDatatypeInfo table, SOP.All2 Column (SOP.Code table)) =>
    proxy table ->
    SQL ()
createTableGeneric proxy =
    SOP.createTableGenericAux (tableNameOf proxy) mkColumnImpls proxy

-- | @insertRowGeneric@ implements 'insertRow' for a 'SOP.Generic' record type.
insertRowGeneric ::
    forall table.
    Table table =>
    (SOP.HasDatatypeInfo table, SOP.All2 Column (SOP.Code table)) =>
    table ->
    SQL (Key table)
insertRowGeneric =
    SOP.insertRowGenericAux (tableNameOf proxy) mkColumnImpls
  where
    proxy = Proxy @table

-- | @selectRowGeneric@ implements 'selectRow' for a 'SOP.Generic' record type.
selectRowGeneric ::
    forall table.
    Table table =>
    (SOP.HasDatatypeInfo table, SOP.All2 Column (SOP.Code table)) =>
    table ->
    SQL (Maybe (Key table))
selectRowGeneric =
    SOP.selectRowGenericAux (tableNameOf proxy) mkColumnImpls
  where
    proxy = Proxy @table

{- | @(createTableIso iso)@ implements 'createTable'.

The table will be named for the @outer@ type (see 'createTableGeneric'). The
'Iso' determines the structure of the table: the table will have the same
columns as the table that 'createTableGeneric' would create for the @inner@
type. Note, however, that the @inner@ type need not have a 'Table' instance!
-}
createTableIso ::
    forall proxy outer inner.
    Table outer =>
    (SOP.HasDatatypeInfo inner, SOP.All2 Column (SOP.Code inner)) =>
    Lens.Iso' outer inner ->
    proxy outer ->
    SQL ()
createTableIso _ proxy =
    SOP.createTableGenericAux tableName mkColumnImpls proxy'
  where
    proxy' = Proxy @inner
    tableName = tableNameOf proxy

{- | @(insertRowIso iso)@ implements 'insertRow' for a table created with
'createTableIso'.
-}
insertRowIso ::
    forall outer inner.
    Table outer =>
    (SOP.HasDatatypeInfo inner, SOP.All2 Column (SOP.Code inner)) =>
    Lens.Iso' outer inner ->
    outer ->
    SQL (Key outer)
insertRowIso iso outer =
    fromInnerKey <$> SOP.insertRowGenericAux tableName mkColumnImpls inner
  where
    tableName = tableNameOf (Proxy @outer)
    inner = Lens.view iso outer
    fromInnerKey :: Key inner -> Key outer
    fromInnerKey = fmap (Lens.review iso)

{- | @(selectRowIso iso)@ implements 'selectRow' for a table created with
'createTableIso'.
-}
selectRowIso ::
    forall outer inner.
    Table outer =>
    (SOP.HasDatatypeInfo inner, SOP.All2 Column (SOP.Code inner)) =>
    Lens.Iso' outer inner ->
    outer ->
    SQL (Maybe (Key outer))
selectRowIso iso outer =
    fromInnerKeys <$> SOP.selectRowGenericAux tableName mkColumnImpls inner
  where
    tableName = tableNameOf (Proxy @outer)
    inner = Lens.view iso outer
    fromInnerKeys :: Maybe (Key inner) -> Maybe (Key outer)
    fromInnerKeys = (fmap . fmap) (Lens.review iso)

{- | @createTableUnwrapped@ implements 'createTable' for a @newtype@ wrapper.

The table will be named for the @outer@ type (see 'createTableGeneric'), but it
will have the same columns as the table that 'createTableGeneric' would create
for the @inner@ type. Note, however, that the @inner@ type need not have a
'Table' instance!

See also: 'createTableIso' is a more general version of this function.
-}
createTableUnwrapped ::
    forall proxy outer inner.
    Table outer =>
    SOP.HasDatatypeInfo inner =>
    SOP.All2 Column (SOP.Code inner) =>
    Wrapped outer outer inner inner =>
    proxy outer ->
    SQL ()
createTableUnwrapped = createTableIso _Unwrapped

-- | @insertRowUnwrapped@ implements 'insertRow' for a @newtype@ wrapper.
insertRowUnwrapped ::
    forall outer inner.
    Table outer =>
    SOP.HasDatatypeInfo inner =>
    SOP.All2 Column (SOP.Code inner) =>
    Wrapped outer outer inner inner =>
    outer ->
    SQL (Key outer)
insertRowUnwrapped = insertRowIso _Unwrapped

-- | @selectRowUnwrapped@ implements 'selectRow' for a @newtype@ wrapper.
selectRowUnwrapped ::
    forall outer inner.
    Table outer =>
    SOP.HasDatatypeInfo inner =>
    SOP.All2 Column (SOP.Code inner) =>
    Wrapped outer outer inner inner =>
    outer ->
    SQL (Maybe (Key outer))
selectRowUnwrapped = selectRowIso _Unwrapped
