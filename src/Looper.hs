{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Looper
  ( LooperDef(..)
  , seconds
  , minutes
  , hours
  , LooperFlags(..)
  , getLooperFlags
  , LooperEnvironment(..)
  , getLooperEnvironment
  , readLooperEnvironment
  , LooperConfiguration(..)
  , LooperSettings(..)
  , deriveLooperSettings
  , mkLooperDef
  , runLoopers
  , runLoopersIgnoreOverrun
  , runLoopersRaw
  , waitNominalDiffTime
  ) where

import GHC.Generics (Generic)

import Control.Applicative
import Control.Monad

import qualified System.Environment as System (getEnvironment)

import Data.Aeson
import Data.Maybe
import Data.String
import Data.Text (Text)
import Data.Time
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif
import Options.Applicative
import Text.Read

import UnliftIO
import UnliftIO.Concurrent

-- | A looper definition
data LooperDef m =
  LooperDef
    { looperDefName :: Text -- ^ The name of the looper, can be useful for logging
    , looperDefEnabled :: Bool -- ^ Whether this looper is enabled
    , looperDefPeriod :: NominalDiffTime -- ^ The time between the start of each run
    , looperDefPhase :: NominalDiffTime -- ^ The time before the first run
    , looperDefFunc :: m () -- ^ The function to run
    }
  deriving (Generic)

-- | Construct a 'NominalDiffTime' from a number of seconds
seconds :: Double -> NominalDiffTime
seconds = realToFrac

-- | Construct a 'NominalDiffTime' from a number of minutes
minutes :: Double -> NominalDiffTime
minutes = seconds . (* 60)

-- | Construct a 'NominalDiffTime' from a number of hours
hours :: Double -> NominalDiffTime
hours = minutes . (* 60)

-- | A structure to parse command-line flags for a looper into
data LooperFlags =
  LooperFlags
    { looperFlagEnabled :: Maybe Bool
    , looperFlagPhase :: Maybe Int -- Seconds
    , looperFlagPeriod :: Maybe Int -- Seconds
    }
  deriving (Show, Eq, Generic)

-- | An optparse applicative parser for 'LooperFlags'
getLooperFlags ::
     String -- ^ The name of the looper (best to make this all-lowercase)
  -> Parser LooperFlags
getLooperFlags name =
  LooperFlags <$> doubleSwitch name (unwords ["enable the", name, "looper"]) mempty <*>
  option
    (Just <$> auto)
    (mconcat
       [ long $ name <> "-phase"
       , metavar "SECONDS"
       , value Nothing
       , help $ unwords ["the phase for the", name, "looper in seconsd"]
       ]) <*>
  option
    (Just <$> auto)
    (mconcat
       [ long $ name <> "-period"
       , metavar "SECONDS"
       , value Nothing
       , help $ unwords ["the period for the", name, "looper in seconds"]
       ])

doubleSwitch :: String -> String -> Mod FlagFields Bool -> Parser (Maybe Bool)
doubleSwitch name helpText mods =
  let enabledValue = True
      disabledValue = False
      defaultValue = True
   in ((last . map Just) <$>
       some
         ((flag'
             enabledValue
             (hidden <> internal <> long ("enable-" ++ name) <> help helpText <> mods) <|>
           flag'
             disabledValue
             (hidden <> internal <> long ("disable-" ++ name) <> help helpText <> mods)) <|>
          flag'
            disabledValue
            (long ("(enable|disable)-" ++ name) <>
             help ("Enable/disable " ++ helpText ++ " (default: " ++ show defaultValue ++ ")") <>
             mods))) <|>
      pure Nothing

-- | A structure to parse environment variables for a looper into
data LooperEnvironment =
  LooperEnvironment
    { looperEnvEnabled :: Maybe Bool
    , looperEnvPhase :: Maybe Int -- Seconds
    , looperEnvPeriod :: Maybe Int -- Seconds
    }
  deriving (Show, Eq, Generic)

-- | Get a 'LooperEnvironment' from the environment
getLooperEnvironment ::
     String -- ^ Prefix for each variable (best to make this all-caps)
  -> String -- ^ Name of the looper (best to make this all-caps too)
  -> IO LooperEnvironment
getLooperEnvironment prefix name = readLooperEnvironment prefix name <$> System.getEnvironment

-- | Get a 'LooperEnvironment' from a pure environment
readLooperEnvironment ::
     String -- ^ Prefix for each variable (best to make this all-caps)
  -> String -- ^ Name of the looper (best to make this all-caps too)
  -> [(String, String)]
  -> LooperEnvironment
readLooperEnvironment prefix name env =
  let v :: IsString s => String -> Maybe s
      v k = fromString <$> lookup (prefix <> k) env
      r :: Read a => String -> Maybe a
      r k = v k >>= readMaybe
      lr :: Read a => String -> Maybe a
      lr k = r $ name <> "_" <> k
   in LooperEnvironment
        { looperEnvEnabled = lr "ENABLED"
        , looperEnvPhase = lr "PHASE"
        , looperEnvPeriod = lr "PERIOD"
        }

-- | A structure to configuration for a looper into
data LooperConfiguration =
  LooperConfiguration
    { looperConfEnabled :: Maybe Bool
    , looperConfPhase :: Maybe Int
    , looperConfPeriod :: Maybe Int
    }
  deriving (Show, Eq, Generic)

-- | You can parse Data.Aeson's JSON or Data.Yaml's YAML to parse a 'LooperConfiguration'.
-- You can also use Data.Yaml.Config.
instance FromJSON LooperConfiguration where
  parseJSON =
    withObject "LooperConfiguration" $ \o ->
      LooperConfiguration <$> o .:? "enabled" <*> o .:? "phase" <*> o .: "period"

-- | Settings that you might want to pass into a looper using 'mkLooperDef'
data LooperSettings =
  LooperSettings
    { looperSetEnabled :: Bool
    , looperSetPhase :: NominalDiffTime
    , looperSetPeriod :: NominalDiffTime
    }
  deriving (Show, Eq, Generic)

deriveLooperSettings ::
     NominalDiffTime -- ^ Default phase
  -> NominalDiffTime -- ^ Default period
  -> LooperFlags
  -> LooperEnvironment
  -> Maybe LooperConfiguration
  -> LooperSettings
deriveLooperSettings defaultPhase defaultPeriod LooperFlags {..} LooperEnvironment {..} mlc =
  let looperSetEnabled =
        fromMaybe True $ looperFlagEnabled <|> looperEnvEnabled <|> (mlc >>= looperConfEnabled)
      looperSetPhase =
        maybe defaultPhase fromIntegral $
        looperFlagPhase <|> looperEnvPhase <|> (mlc >>= looperConfPhase)
      looperSetPeriod =
        maybe defaultPeriod fromIntegral $
        looperFlagPeriod <|> looperEnvPeriod <|> (mlc >>= looperConfPeriod)
   in LooperSettings {..}

mkLooperDef ::
     Text -- ^ Name
  -> LooperSettings
  -> m () -- ^ The function to loop
  -> LooperDef m
mkLooperDef name LooperSettings {..} func =
  LooperDef
    { looperDefName = name
    , looperDefEnabled = looperSetEnabled
    , looperDefPeriod = looperSetPeriod
    , looperDefPhase = looperSetPhase
    , looperDefFunc = func
    }

-- | Simply run loopers
--
-- > runLoopers = runLoopersIgnoreOverrun looperDefFunc
--
-- see 'runLoopersIgnoreOverrun'
--
-- Note that this function will loop forever, you need to wrap it using 'async' yourself.
runLoopers :: MonadUnliftIO m => [LooperDef m] -> m ()
runLoopers = runLoopersIgnoreOverrun looperDefFunc

-- | Run loopers with a custom runner, ignoring any overruns
--
-- > runLoopersIgnoreOverrun = runLoopersRaw (pure ())
--
-- see 'runLoopersRaw'
--
-- Note that this function will loop forever, you need to wrap it using 'async' yourself.
runLoopersIgnoreOverrun ::
     MonadUnliftIO m
  => (LooperDef m -> m ()) -- ^ Custom runner
  -> [LooperDef m] -- ^ Loopers
  -> m ()
runLoopersIgnoreOverrun = runLoopersRaw (const $ pure ())

-- | Run loopers, with a custom runner and overrun handler
--
-- * The overrun handler is run when the looper function takes longer than its period.
--   You can use this to log a warning, for example.
--
-- * The runner function is used to run the looper function
--   You can use 'looperDefFunc' @ :: LooperDef m -> m ()@ to run a 'LooperDef', and you
--   can wrap this function in some custom logic before you pass it into 'runLoopersRaw'
--   In this manner you can add logging or metrics, for example.
--
-- Note that this function will loop forever, you need to wrap it using 'async' yourself.
runLoopersRaw ::
     MonadUnliftIO m
  => (LooperDef m -> m ()) -- ^ Overrun handler
  -> (LooperDef m -> m ()) -- ^ Runner
  -> [LooperDef m] -- ^ Loopers
  -> m ()
runLoopersRaw onOverrun runLooper =
  mapConcurrently_ $ \ld@LooperDef {..} ->
    when looperDefEnabled $ do
      waitNominalDiffTime looperDefPhase
      let loop = do
            start <- liftIO $ getCurrentTime
            runLooper ld
            end <- liftIO $ getCurrentTime
            let elapsed = diffUTCTime end start
            let nextWait = looperDefPeriod - elapsed
            if (nextWait < 0)
              then onOverrun ld
              else waitNominalDiffTime nextWait
            loop
      loop

-- | Wait for a given 'NominalDiffTime'
--
-- This takes care of the conversion to microseconds to pass to 'threadDelay' for you.
--
-- > waitNominalDiffTime ndt = liftIO $ threadDelay $ round (toRational ndt * (1000 * 1000))
waitNominalDiffTime :: MonadIO m => NominalDiffTime -> m ()
waitNominalDiffTime ndt = liftIO $ threadDelay $ round (toRational ndt * (1000 * 1000))
