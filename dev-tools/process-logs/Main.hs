{- | Log processing utility

Copyright   : (c) Runtime Verification, 2024
License     : BSD-3-Clause
-}
module Main (
    main,
) where

import Control.Monad (unless)
import Data.Aeson qualified as JSON
import Data.Aeson.Encode.Pretty qualified as JSON
import Data.ByteString.Char8 qualified as BSS
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.List (foldl', maximumBy, sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Time.Clock
import Data.Time.Clock.System (systemToUTCTime)
import Options.Applicative
import System.Exit
import Text.Printf

import Booster.Log.Context (ContextFilter, mustMatch, readContextFilter)
import Kore.JsonRpc.Types (rpcJsonConfig)
import Kore.JsonRpc.Types.ContextLog

-- reads log file in json-format from stdin (or a single given file)
-- applies the command
-- outputs resulting log file or resulting data to stdout or a given file
main :: IO ()
main = do
    Options{cmd, input, output} <- execParser parse
    (errors, inputJson) <-
        partitionEithers
            . map JSON.eitherDecode
            . BS.lines
            <$> maybe BS.getContents BS.readFile input
    unless (null errors) $ do
        putStrLn "JSON parse errors in log file:"
        mapM_ putStrLn errors
        exitWith (ExitFailure 1)
    let writeOut = maybe BS.putStrLn BS.writeFile output . BS.unlines
    writeOut $ process cmd inputJson

data Options = Options
    { cmd :: Command
    , input :: Maybe FilePath
    , output :: Maybe FilePath
    }
    deriving (Show)

data Command
    = -- | filter a log file, output to stdout. Same options as for the server
      Filter [ContextFilter]
    | -- | find repeated rule/equation contexts in lines
      FindRecursions
    | -- | compute total times spent on applying certain rules/equations (top-level)
      TimesPerRule
    deriving (Show)

parse :: ParserInfo Options
parse =
    info
        (parseOpts <**> helper)
        (fullDesc <> progDesc "log-processing utility for json context logs")
  where
    parseOpts =
        Options
            <$> commandParser
            <*> optional
                ( strOption
                    ( long "input-file"
                        <> short 'i'
                        <> metavar "INPUTFILE"
                        <> help "take input from a file instead of stdin"
                    )
                )
            <*> optional
                ( strOption
                    ( long "output-file"
                        <> short 'o'
                        <> metavar "OUTPUTFILE"
                        <> help "write output to a file instead of stdout"
                    )
                )
    commandParser =
        subparser $
            ( command
                "filter"
                ( info
                    ((Filter <$> many parseContextFilter) <**> helper)
                    (progDesc "filter log file with given contexts (space separated)")
                )
            )
                <> ( command
                        "find-recursions"
                        ( info
                            (pure FindRecursions <**> helper)
                            (progDesc "find repeated contexts in log lines")
                        )
                   )
                <> ( command
                        "times-per-rule"
                        ( info
                            (pure TimesPerRule <**> helper)
                            (progDesc "compute total times spent per (top-level) rule/equation")
                        )
                   )

    parseContextFilter =
        argument
            (eitherReader readContextFilter)
            ( metavar "CONTEXT"
                <> help "Log context"
            )

------------------------------------------------------------

process :: Command -> [LogLine] -> [BS.ByteString]
process (Filter filters) =
    map encodeLogLine . filterLines filters
process FindRecursions =
    (heading <>) . map renderResult . findRecursions
  where
    heading =
        [ "| Context                | Longest | Count | Prefix"
        , "|----------------------- | ------- | ----- |-----------"
        ]
    renderResult (ctx, (pfx, len, cnt)) =
        BS.pack $ printf "| %22s | %7d | %5d | %s" (show ctx) len cnt (showCtx pfx)

    showCtx = concatMap (show . (: []))
process TimesPerRule =
    (heading <>) . map renderResult . ruleStatistics
  where
    heading =
        [ "| Rule/Equation          | Success             | Failure             | Abort"
        , "|----------------------- | ------------------- | ------------------- | -------------------"
        ]
    renderResult :: (IdContext, RuleStats) -> BS.ByteString
    renderResult (ctx, stats) =
        BS.pack $
            printf
                "| %22s | %10.6fs (%5d) | %10.6fs (%5d) | %10.6fs (%5d)"
                (show ctx)
                stats.tSuccess
                stats.nSuccess
                stats.tFailure
                stats.nFailure
                stats.tAbort
                stats.nAbort

encodeLogLine :: LogLine -> BS.ByteString
encodeLogLine = JSON.encodePretty' rpcJsonConfig{JSON.confIndent = JSON.Spaces 0}

------------------------------------------------------------
filterLines :: [ContextFilter] -> [LogLine] -> [LogLine]
filterLines filters = filter keepLine
  where
    keepLine LogLine{context} =
        let cs = map (BSS.pack . show) $ toList context
         in matchesAFilter cs
    matchesAFilter :: [BSS.ByteString] -> Bool
    matchesAFilter x = any (flip mustMatch x) filters

------------------------------------------------------------
lineRecursion :: LogLine -> Maybe (CLContext, ([CLContext], Int))
lineRecursion LogLine{context}
    | null repeatedContexts = Nothing
    | otherwise = Just (maxRepeatC, (prefix, count + 1))
  where
    repeatedContexts = rr context
    rr Seq.Empty = []
    rr (c :<| cs)
        | CLWithId (c') <- c -- only contexts with ID (rules, equations, hooks)
        , isRuleCtx c'
        , repeats > 0 =
            (c, repeats) : rr cs
        | otherwise = rr cs
      where
        repeats = length $ Seq.filter (== c) cs

    (maxRepeatC, count) = maximumBy (comparing snd) repeatedContexts

    prefix = takeWhile (/= maxRepeatC) $ toList context

isRuleCtx :: IdContext -> Bool
isRuleCtx CtxFunction{} = True
isRuleCtx CtxSimplification{} = True
isRuleCtx CtxRewrite{} = True
isRuleCtx _ = False

findRecursions :: [LogLine] -> [(CLContext, ([CLContext], Int, Int))]
findRecursions ls = Map.assocs resultMap
  where
    recursions =
        [(ctx, (pfx, cnt, 1)) | (ctx, (pfx, cnt)) <- mapMaybe lineRecursion ls]
    maxAndCount ::
        ([CLContext], Int, Int) ->
        ([CLContext], Int, Int) ->
        ([CLContext], Int, Int)
    maxAndCount (pfx1, len1, cnt1) (pfx2, len2, cnt2)
        | len1 >= len2 =
            (pfx1, len1, cnt1 + cnt2)
        | otherwise =
            (pfx2, len2, cnt1 + cnt2)
    resultMap =
        foldl' (\m (ctx, item) -> Map.insertWith maxAndCount ctx item m) mempty recursions

------------------------------------------------------------
-- rule statistics

ruleStatistics :: [LogLine] -> [(IdContext, RuleStats)]
ruleStatistics =
    sortBy (comparing (Down . allTimes . snd))
        . Map.assocs
        . ruleStats
  where
    allTimes :: RuleStats -> Double
    allTimes stats = stats.tSuccess + stats.tFailure + stats.tAbort

data RuleStats = RuleStats
    { -- counts of:
      nSuccess :: !Int -- successful application
    , nFailure :: !Int -- failure to apply
    , nAbort :: !Int -- failure, leading to abort
    , -- total times for these categories
      tSuccess :: !Double
    , tFailure :: !Double
    , tAbort :: !Double
    }
    deriving stock (Eq, Ord, Show)

instance Monoid RuleStats where
    mempty = RuleStats 0 0 0 0 0 0

instance Semigroup RuleStats where
    rStats1 <> rStats2 =
        RuleStats
            { nSuccess = rStats1.nSuccess + rStats2.nSuccess
            , nFailure = rStats1.nFailure + rStats2.nFailure
            , nAbort = rStats1.nAbort + rStats2.nAbort
            , tSuccess = rStats1.tSuccess + rStats2.tSuccess
            , tFailure = rStats1.tFailure + rStats2.tFailure
            , tAbort = rStats1.tAbort + rStats2.tAbort
            }

ruleStats :: [LogLine] -> Map IdContext RuleStats
ruleStats = Map.fromListWith (<>) . collect
  where
    collect [] = []
    collect (l@LogLine{context} : ls)
        | Seq.null rulePart -- no rule involved?
            =
            collect ls
        | otherwise =
            let (outcome, rest) = fromCtxSpan (prefix :|> ruleCtx) (l : ls)
             in (ruleId, outcome) : collect rest
      where
        (prefix, rulePart) = Seq.breakl interesting context
        (ruleCtx, ruleId) = case rulePart of
            hd :<| _rest
                | c@(CLWithId c') <- hd -> (c, c')
                | CLNullary{} <- hd -> error "no rule head found"
            Seq.Empty -> error "no rule head found"

    -- only contexts with ID (rules, equations, hooks)
    interesting CLNullary{} = False
    interesting (CLWithId c') = isRuleCtx c'

    fromCtxSpan :: Seq CLContext -> [LogLine] -> (RuleStats, [LogLine])
    fromCtxSpan prefix ls
        | null prefixLines =
            error "Should have at least one line with the prefix" -- see above
        | otherwise =
            (mkOutcome (head prefixLines) (last prefixLines), rest)
      where
        len = Seq.length prefix

        hasPrefix :: LogLine -> Bool
        hasPrefix = (== prefix) . Seq.take len . (.context)

        (prefixLines, rest) = span hasPrefix ls

        mkOutcome :: LogLine -> LogLine -> RuleStats
        mkOutcome startLine endLine =
            let time =
                    maybe
                        1
                        realToFrac
                        ( diffUTCTime
                            <$> fmap systemToUTCTime endLine.timestamp
                            <*> fmap systemToUTCTime startLine.timestamp
                        )
             in case Seq.drop len endLine.context of
                    CLNullary CtxSuccess :<| _ ->
                        RuleStats 1 0 0 time 0 0
                    -- rewrite failures
                    _ :|> CLNullary CtxFailure ->
                        RuleStats 0 1 0 0 time 0
                    _ :|> CLNullary CtxIndeterminate ->
                        RuleStats 0 0 1 0 0 time
                    -- equation failures
                    _ :|> CLNullary CtxContinue ->
                        RuleStats 0 1 0 0 time 0
                    _ :|> CLNullary CtxBreak ->
                        RuleStats 0 0 1 0 0 time
                    other ->
                        -- case not covered...
                        error $ "Unexpected last context " <> show other
