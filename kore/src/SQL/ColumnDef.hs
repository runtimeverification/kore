{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module SQL.ColumnDef (
    TypeName,
    getTypeName,
    typeInteger,
    typeText,
    ColumnConstraint,
    getColumnConstraint,
    notNull,
    primaryKey,
    ColumnDef (..),
    columnDef,
    columnNotNull,
) where

import Control.Lens qualified as Lens
import Data.Binary (Binary)
import Data.Generics.Product.Fields
import Data.Set (
    Set,
 )
import Data.Set qualified as Set
import GHC.Generics qualified as GHC
import Prelude.Kore

newtype TypeName = TypeName {getTypeName :: String}
    deriving stock (Eq, Ord, Read, Show)
    deriving newtype (Binary)

typeInteger :: TypeName
typeInteger = TypeName "INTEGER"

typeText :: TypeName
typeText = TypeName "TEXT"

newtype ColumnConstraint = ColumnConstraint {getColumnConstraint :: String}
    deriving stock (Eq, Ord, Read, Show)
    deriving newtype (Binary)

notNull :: Set ColumnConstraint
notNull = Set.singleton (ColumnConstraint "NOT NULL")

primaryKey :: Set ColumnConstraint
primaryKey = Set.singleton (ColumnConstraint "PRIMARY KEY")

data ColumnDef = ColumnDef
    { columnType :: !TypeName
    , columnConstraints :: !(Set ColumnConstraint)
    }
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)

columnDef :: TypeName -> ColumnDef
columnDef columnType = ColumnDef{columnType, columnConstraints = mempty}

columnNotNull :: ColumnDef -> ColumnDef
columnNotNull = Lens.over (field @"columnConstraints") (<> notNull)
