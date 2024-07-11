{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.WarnIfLowProductivity (
    WarnIfLowProductivity (..),
    warnIfLowProductivity,
) where

import Kore.Attribute.Definition (KFileLocations (..))
import Log
import Numeric.Natural
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified
import Stats

{- | @WarnIfLowProductivity@ is emitted when productuvity drops below a certain
point.

The warning message also displays the locations of the original K files used if
they are provided as attributes in the kore file.
-}
data WarnIfLowProductivity = WarnIfLowProductivity
    { productivityPercent :: Natural
    , kFileLocations :: KFileLocations
    }
    deriving stock (Show)

instance Pretty WarnIfLowProductivity where
    pretty
        WarnIfLowProductivity
            { productivityPercent
            , kFileLocations = KFileLocations locations
            } =
            (Pretty.vsep . concat)
                [
                    [ Pretty.hsep
                        [ "Productivity dropped to:"
                        , Pretty.pretty productivityPercent <> "%"
                        ]
                    ]
                , kFiles
                ,
                    [ "Poor productivity may indicate a performance bug."
                    , "Please file a bug report: https://github.com/runtimeverification/haskell-backend/issues"
                    ]
                ]
          where
            kFiles
                | not . null $ locations =
                    [ (Pretty.nest 4 . Pretty.vsep)
                        ("Relevant K files include:" : fmap Pretty.pretty locations)
                    ]
                | otherwise = []
instance Entry WarnIfLowProductivity where
    entrySeverity _ = Warning
    oneLineDoc (WarnIfLowProductivity productivityPercent _) =
        Pretty.pretty productivityPercent
    oneLineContextDoc _ = single CtxWarn
    helpDoc _ = "warn when productivty (MUT time / Total time) drops below 90%"

warnIfLowProductivity ::
    MonadLog log =>
    MonadIO log =>
    KFileLocations ->
    log ()
warnIfLowProductivity kFileLocations = do
    Stats{gc_cpu_ns, cpu_ns} <- liftIO getStats
    let gcTimeOver10Percent = gc_cpu_ns * 10 > cpu_ns
        gcPercentage = gc_cpu_ns * 100 `div` cpu_ns
        productivity = 100 - gcPercentage & fromIntegral
        runTimeOver60Seconds = cpu_ns >= 60 * 10 ^ (9 :: Int)
    when (runTimeOver60Seconds && gcTimeOver10Percent) . logEntry $
        WarnIfLowProductivity productivity kFileLocations
