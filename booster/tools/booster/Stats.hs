module Stats (
    newStats,
    addStats,
    finaliseStats,
    timed,
    secWithUnit,
    RequestStats (..),
    StatsVar,
    MethodTiming (..),
) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (pack)
import Deriving.Aeson
import GHC.Generics ()
import Prettyprinter
import System.Clock
import Text.Printf

import Booster.Log
import Booster.Prettyprinter
import Kore.JsonRpc.Types (APIMethod)

-- | Statistics for duration measurement time series (in seconds)
data RequestStats a = RequestStats
    { count :: Int
    , average :: a
    , stddev :: a
    , total :: a
    , maxVal :: a
    , minVal :: a
    , koreTotal :: a
    , koreAverage :: a
    , koreMax :: a
    }
    deriving stock (Eq, Show, Generic)
    deriving
        (FromJSON, ToJSON)
        via CustomJSON '[FieldLabelModifier '[CamelToKebab]] (RequestStats a)

instance (Floating a, PrintfArg a, Ord a) => Pretty (RequestStats a) where
    pretty stats =
        vsep
            [ "Requests: " <> pretty stats.count
            , "Total time: " <> withUnit stats.total
            , "Average time per request:"
                <+> withUnit stats.average
                <+> parens ("+-" <+> withUnit stats.stddev)
                    <> ", range"
                <+> brackets (withUnit stats.minVal <> ", " <> withUnit stats.maxVal)
            , "Total time in kore-rpc code:"
                <+> withUnit stats.koreTotal
            , "Average time per request in kore-rpc code:"
                <+> withUnit stats.koreAverage
                    <> ", max"
                <+> withUnit stats.koreMax
            ]
      where
        withUnit = pretty . secWithUnit

secWithUnit :: (Floating a, Ord a, PrintfArg a) => a -> String
secWithUnit x
    | x > 0.1 = printf "%.2fs" x
    | x > 0.0001 = printf "%.3fms" $ x * 10 ** 3
    | otherwise = printf "%.1fμs" $ x * 10 ** 6

-- internal helper type
-- all values are in seconds
data Stats' = Stats'
    { count :: Int
    , total :: Double
    , squares :: Double
    , maxVal :: Double
    , minVal :: Double
    , koreTotal :: Double
    , koreMax :: Double
    }

instance Semigroup Stats' where
    (<>) = addStats'

addStats' :: Stats' -> Stats' -> Stats'
addStats' stats1 stats2 =
    Stats'
        { count = stats1.count + stats2.count
        , total = stats1.total + stats2.total
        , squares = stats1.squares + stats2.squares
        , maxVal = max stats1.maxVal stats2.maxVal
        , minVal = min stats1.minVal stats2.minVal
        , koreTotal = stats1.koreTotal + stats2.koreTotal
        , koreMax = max stats1.koreMax stats2.koreMax
        }

singleStats' :: Double -> Double -> Stats'
singleStats' x korePart =
    Stats'
        { count = 1
        , total = x
        , squares = x * x
        , maxVal = x
        , minVal = x
        , koreTotal = korePart
        , koreMax = korePart
        }

type StatsVar = MVar (Map APIMethod Stats')

-- helper type mainly for json logging
data MethodTiming = MethodTiming {method :: APIMethod, time :: Double, koreTime :: Double}
    deriving stock (Eq, Show, Generic)
    deriving
        (ToJSON, FromJSON)
        via CustomJSON '[FieldLabelModifier '[CamelToKebab]] MethodTiming

instance ToLogFormat MethodTiming where
    toTextualLog mt =
        pack $
            printf
                "Performed %s in %s (%s kore time)"
                (show mt.method)
                (secWithUnit mt.time)
                (secWithUnit mt.koreTime)
    toJSONLog = toJSON

addStats ::
    MonadIO m =>
    MVar (Map APIMethod Stats') ->
    MethodTiming ->
    m ()
addStats statVar MethodTiming{method, time, koreTime} =
    liftIO . modifyMVar_ statVar $
        pure . Map.insertWith (<>) method (singleStats' time koreTime)

newStats :: MonadIO m => m (MVar (Map APIMethod Stats'))
newStats = liftIO $ newMVar Map.empty

-- returns time taken by the given action (in seconds)
timed :: MonadIO m => m a -> m (a, Double)
timed action = do
    start <- liftIO $ getTime Monotonic
    result <- action
    stop <- liftIO $ getTime Monotonic
    let time = fromIntegral (toNanoSecs (diffTimeSpec stop start)) / 10 ** 9
    pure (result, time)

newtype FinalStats = FinalStats (Map APIMethod (RequestStats Double))
    deriving stock (Eq, Show)
    deriving newtype (FromJSON, ToJSON)

instance Pretty FinalStats where
    pretty (FinalStats stats) =
        vsep $
            [ "---------------------------"
            , "RPC request time statistics"
            , "---------------------------"
            ]
                <> map (\(k, v) -> hang 4 $ vsep [pretty $ show k <> ": ", pretty v]) (Map.assocs stats)

instance ToLogFormat FinalStats where
    toTextualLog = renderText . pretty
    toJSONLog = toJSON

finaliseStats :: MVar (Map APIMethod Stats') -> IO FinalStats
finaliseStats var = FinalStats . Map.map finalise <$> readMVar var
  where
    finalise :: Stats' -> RequestStats Double
    finalise Stats'{count, total, squares, maxVal, minVal, koreTotal, koreMax} =
        let average = total / fromIntegral count
            stddev = sqrt $ squares / fromIntegral count - average * average
            koreAverage = koreTotal / fromIntegral count
         in RequestStats
                { count
                , total
                , average
                , stddev
                , maxVal
                , minVal
                , koreTotal
                , koreAverage
                , koreMax
                }
