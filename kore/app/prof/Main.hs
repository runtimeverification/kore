module Main (main) where

import Data.Proxy
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Kore.Util.TSM qualified as TSM
import Kore.Util.TSM.UnifyTag (UnifyTag, CheckImplTag)
import Options.Applicative as Options
import Prelude.Kore
import Speedscope.Main qualified as Speedscope
import System.IO (hFlush, stdout)

main :: IO ()
main =
    Options.execParser inf
        >>= \case
            Speedscope opts -> Speedscope.main opts
            TSM opts -> runTsm opts
  where
    inf =
        Options.info
            parser
            ( mconcat
                [ Options.fullDesc
                , Options.progDesc "Write a speedscope profile for FILENAME"
                , Options.header "kore-prof - Kore profiler"
                ]
            )

parser :: Parser Mode
parser =
    ( Speedscope <$> Speedscope.parseOptions
        <|> TSM <$> parseTSMMode
    )
        <**> helper

----------------------------------------
-- Timing State Machine mode

data Mode
    = Speedscope Speedscope.Options
    | TSM TSMOptions

data TSMOptions = TSMOptions
    { component :: String
    , mbTsmOutputFile :: Maybe FilePath
    , tsmSourceFile :: FilePath
    }

parseTSMMode :: Parser TSMOptions
parseTSMMode =
    TSMOptions
        <$> option
            parseComponentTag
            -- flag to switch on TSM mode
            ( long "timing-state-machine"
                <> metavar "COMPONENT"
                <> help
                    ( "Build timing state machine (TSM) for the component (known components: "
                        <> show (map fst components)
                        <> ")"
                    )
            )
        <*> optional
            ( strOption
                ( short 'o'
                    <> long "output"
                    <> metavar "OUTPUT_FILE"
                    <> help "TSM output file (default: FILENAME.COMPONENT.dot)"
                )
            )
        <*> argument
            str
            ( metavar "FILENAME"
                <> help "eventlog file"
            )
  where
    parseComponentTag = do
        componentStr <- str
        unless (componentStr `elem` map fst components) $
            error ("Unknown component " <> componentStr)
        return componentStr

-- implements the type lookup to select the component
components :: [(String, FilePath -> IO String)]
components =
    [ ("Unify", TSM.graphOf (Proxy @UnifyTag))
    , ("CheckImplication", TSM.graphOf (Proxy @CheckImplTag))
    ]

runTsm :: TSMOptions -> IO ()
runTsm TSMOptions{component, mbTsmOutputFile, tsmSourceFile} = do
    setLocaleEncoding utf8
    putStr $ tsmSourceFile <> "..."
    hFlush stdout
    let worker =
            -- validity checked before
            fromMaybe (error "unknown component") $ lookup component components
    contents <- worker tsmSourceFile
    let outputFile =
            fromMaybe
                (tsmSourceFile <> "." <> component <> ".dot")
                mbTsmOutputFile
    writeFile outputFile contents
    putStrLn "Done"
