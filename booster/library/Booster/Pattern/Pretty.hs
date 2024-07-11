{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Booster.Pattern.Pretty (
    -- export everything, modules above can re-export only type names
    module Booster.Pattern.Pretty,
) where

import Data.ByteString (ByteString)
import Data.Text.Encoding qualified as Text
import Prettyprinter (Pretty (..), (<+>))
import Prettyprinter qualified as Pretty

import Booster.Definition.Attributes.Base (NotPreservesDefinednessReason (UndefinedSymbol))
import Booster.Pattern.Base
import Booster.Prettyprinter qualified as KPretty
import Booster.Util (decodeLabel')
import Data.ByteString.Char8 qualified as BS
import Data.Data (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as Text

data ModifierT = Truncated | Infix | Decoded deriving (Show)

data Modifiers = Modifiers
    { isTruncated, isInfix, isDecoded :: Bool
    }

defaultModifiers :: Modifiers
defaultModifiers = Modifiers False False False

class FromModifierT (m :: ModifierT) where
    modifiers' :: Modifiers -> Modifiers

instance FromModifierT 'Truncated where
    modifiers' m = m{isTruncated = True}

instance FromModifierT 'Infix where
    modifiers' m = m{isInfix = True}

instance FromModifierT 'Decoded where
    modifiers' m = m{isDecoded = True}

class FromModifiersT (m :: [ModifierT]) where
    modifiers :: Modifiers -> Modifiers

instance FromModifiersT '[] where
    modifiers = id

instance (FromModifierT m, FromModifiersT ms) => FromModifiersT (m ': ms) where
    modifiers = modifiers @ms . modifiers' @m

data ModifiersRep = forall mods. FromModifiersT mods => ModifiersRep (Proxy mods)

toModifiersRep :: [ModifierT] -> ModifiersRep
toModifiersRep = go (ModifiersRep @'[] Proxy)
  where
    go rep@(ModifiersRep (Proxy :: Proxy mods)) = \case
        [] -> rep
        (Truncated : xs) -> go (ModifiersRep @('Truncated ': mods) Proxy) xs
        (Infix : xs) -> go (ModifiersRep @('Infix ': mods) Proxy) xs
        (Decoded : xs) -> go (ModifiersRep @('Decoded ': mods) Proxy) xs

data PrettyWithModifiers (ms :: [ModifierT]) a = FromModifiersT ms => PrettyWithModifiers a

-- used for printing the string as it appears (with codepoints)
prettyBS :: ByteString -> Pretty.Doc a
prettyBS = Pretty.pretty . Text.decodeUtf8

pretty' ::
    forall ms a ann. (Pretty (PrettyWithModifiers ms a), FromModifiersT ms) => a -> Pretty.Doc ann
pretty' = pretty @(PrettyWithModifiers ms a) . PrettyWithModifiers

instance Pretty (PrettyWithModifiers mods Term) where
    pretty (PrettyWithModifiers t) = case t of
        AndTerm t1 t2 ->
            pretty' @mods t1 <+> "/\\" <+> pretty' @mods t2
        SymbolApplication symbol _sortParams args | not isInfix -> symbolApp symbol args
        SymbolApplication symbol _sortParams args
            | otherwise ->
                if BS.count '_' (decodeLabel' symbol.name) >= length args
                    then
                        let split = Text.splitOn "_" $ Text.replace "Lbl" "" $ Text.decodeUtf8 $ decodeLabel' symbol.name
                         in Pretty.hsep $ interleaveArgs split $ map (pretty' @mods) args
                    else symbolApp symbol args
        DotDotDot -> "..."
        DomainValue _sort bs -> pretty . show . Text.decodeLatin1 . shortenBS $ bs
        Var var -> pretty' @mods var
        Injection _source _target t' -> pretty' @mods t'
        KMap _attrs keyVals rest ->
            Pretty.braces . Pretty.hsep . Pretty.punctuate Pretty.comma $
                [pretty' @mods k <+> "->" <+> pretty' @mods v | (k, v) <- keyVals]
                    ++ maybe [] ((: []) . pretty' @mods) rest
        KList _meta heads (Just (mid, tails)) ->
            Pretty.hsep $
                Pretty.punctuate
                    " +"
                    [renderList heads, pretty' @mods mid, renderList tails]
        KList _meta [] Nothing ->
            "[]"
        KList _meta heads Nothing ->
            renderList heads
        KSet _meta [] Nothing -> "{}"
        KSet _meta [] (Just rest) -> pretty' @mods rest
        KSet _meta es rest ->
            (Pretty.braces . Pretty.hsep . Pretty.punctuate Pretty.comma $ map (pretty' @mods) es)
                Pretty.<+> maybe mempty ((" ++ " <>) . pretty' @mods) rest
      where
        Modifiers
            { isTruncated
            , isInfix
            , isDecoded
            } = modifiers @mods defaultModifiers

        symbolApp symbol args =
            pretty
                ( if isDecoded
                    then Text.replace "Lbl" "" $ Text.decodeUtf8 $ decodeLabel' symbol.name
                    else Text.decodeUtf8 symbol.name
                )
                <> KPretty.arguments' (map (pretty' @mods) args)
        renderList l
            | null l = mempty
            | otherwise =
                Pretty.brackets . Pretty.hsep . Pretty.punctuate Pretty.comma $
                    map (pretty' @mods) l

        interleaveArgs [] _ = []
        interleaveArgs (s : _) [] = [pretty s | not (Text.null s)]
        interleaveArgs (s : ss) (a : as) | Text.null s = a : interleaveArgs ss as
        interleaveArgs (s : ss) (a : as) = pretty s : a : interleaveArgs ss as

        -- shorten domain value ByteString to a readable length
        shortenBS dv =
            let cutoff = 16
             in if isTruncated
                    then if BS.length dv < cutoff then dv else BS.take cutoff dv <> "...truncated"
                    else dv

instance Pretty (PrettyWithModifiers mods Sort) where
    pretty (PrettyWithModifiers (SortApp name params)) =
        prettyBS name <> KPretty.parameters' (map (pretty' @mods) params)
    pretty (PrettyWithModifiers (SortVar name)) =
        prettyBS name

instance FromModifiersT mods => Pretty (PrettyWithModifiers mods Variable) where
    pretty :: PrettyWithModifiers mods Variable -> Pretty.Doc ann
    pretty (PrettyWithModifiers var) =
        prettyBS (if isDecoded then decodeLabel' var.variableName else var.variableName)
            <> Pretty.colon
            <> pretty' @mods var.variableSort
      where
        Modifiers
            { isDecoded
            } = modifiers @mods defaultModifiers

instance FromModifiersT mods => Pretty (PrettyWithModifiers mods Predicate) where
    pretty (PrettyWithModifiers (Predicate t)) = pretty' @mods t

instance FromModifiersT mods => Pretty (PrettyWithModifiers mods Ceil) where
    pretty (PrettyWithModifiers (Ceil t)) =
        "\\ceil"
            <> KPretty.noParameters
            <> KPretty.arguments' [pretty' @mods t]

instance FromModifiersT mods => Pretty (PrettyWithModifiers mods Pattern) where
    pretty (PrettyWithModifiers patt) =
        Pretty.vsep $
            [ "Term:"
            , Pretty.indent 4 $ pretty' @mods patt.term
            , "Conditions:"
            ]
                <> fmap (Pretty.indent 4) (map (pretty' @mods) $ Set.toList patt.constraints)
                <> fmap (Pretty.indent 4) (map (pretty' @mods) patt.ceilConditions)

instance FromModifiersT mods => Pretty (PrettyWithModifiers mods NotPreservesDefinednessReason) where
    pretty = \case
        PrettyWithModifiers (UndefinedSymbol name) ->
            pretty $
                if isDecoded
                    then Text.replace "Lbl" "" $ Text.decodeUtf8 $ decodeLabel' name
                    else Text.decodeUtf8 name
      where
        Modifiers
            { isDecoded
            } = modifiers @mods defaultModifiers
