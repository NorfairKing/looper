{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Looper
  ( LooperDef (..),
    seconds,
    minutes,
    hours,
    LooperFlags (..),
    getLooperFlags,
    LooperEnvironment (..),
    getLooperEnvironment,
    readLooperEnvironment,
    looperEnvironmentParser,
    LooperConfiguration (..),
    LooperSettings (..),
    deriveLooperSettings,
    mkLooperDef,
    runLoopers,
    runLoopersIgnoreOverrun,
    runLoopersRaw,
    waitNominalDiffTime,
  )
where

import Autodocodec
import Control.Applicative
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe
import Data.Text (Text)
import Data.Time
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import qualified System.Environment as System (getEnvironment)
import UnliftIO
import UnliftIO.Concurrent

-- | A looper definition
data LooperDef m = LooperDef
  { -- | The name of the looper, can be useful for logging
    looperDefName :: Text,
    -- | Whether this looper is enabled
    looperDefEnabled :: Bool,
    -- | The time between the start of each run
    looperDefPeriod :: NominalDiffTime,
    -- | The time before the first run
    looperDefPhase :: NominalDiffTime,
    -- | The function to run
    looperDefFunc :: m ()
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
data LooperFlags = LooperFlags
  { looperFlagEnabled :: Maybe Bool,
    looperFlagPhase :: Maybe Word, -- Seconds
    looperFlagPeriod :: Maybe Word -- Seconds
  }
  deriving (Show, Eq, Generic)

-- | An optparse applicative parser for 'LooperFlags'
getLooperFlags ::
  -- | The name of the looper (best to make this all-lowercase)
  String ->
  OptParse.Parser LooperFlags
getLooperFlags name =
  LooperFlags <$> doubleSwitch name (unwords ["enable the", name, "looper"]) mempty
    <*> option
      (Just <$> auto)
      ( mconcat
          [ long $ name <> "-phase",
            metavar "SECONDS",
            value Nothing,
            help $ unwords ["the phase for the", name, "looper in seconsd"]
          ]
      )
    <*> option
      (Just <$> auto)
      ( mconcat
          [ long $ name <> "-period",
            metavar "SECONDS",
            value Nothing,
            help $ unwords ["the period for the", name, "looper in seconds"]
          ]
      )

doubleSwitch :: String -> String -> Mod FlagFields Bool -> OptParse.Parser (Maybe Bool)
doubleSwitch name helpText mods =
  let enabledValue = True
      disabledValue = False
      defaultValue = True
   in ( last . map Just
          <$> some
            ( ( flag'
                  enabledValue
                  (hidden <> internal <> long ("enable-" ++ name) <> help helpText <> mods)
                  <|> flag'
                    disabledValue
                    (hidden <> internal <> long ("disable-" ++ name) <> help helpText <> mods)
              )
                <|> flag'
                  disabledValue
                  ( long ("(enable|disable)-" ++ name)
                      <> help ("Enable/disable " ++ helpText ++ " (default: " ++ show defaultValue ++ ")")
                      <> mods
                  )
            )
      )
        <|> pure Nothing

-- | A structure to parse environment variables for a looper into
data LooperEnvironment = LooperEnvironment
  { looperEnvEnabled :: Maybe Bool,
    looperEnvPhase :: Maybe Word, -- Seconds
    looperEnvPeriod :: Maybe Word -- Seconds
  }
  deriving (Show, Eq, Generic)

-- | Get a 'LooperEnvironment' from the environment
getLooperEnvironment ::
  -- | Prefix for each variable (best to make this all-caps)
  String ->
  -- | Name of the looper (best to make this all-caps too)
  String ->
  IO LooperEnvironment
getLooperEnvironment prefix name = readLooperEnvironment prefix name <$> System.getEnvironment

-- | Get a 'LooperEnvironment' from a pure environment
readLooperEnvironment ::
  -- | Prefix for each variable (best to make this all-caps)
  String ->
  -- | Name of the looper (best to make this all-caps too)
  String ->
  [(String, String)] ->
  LooperEnvironment
readLooperEnvironment prefix name env = case Env.parsePure (Env.prefixed prefix $ looperEnvironmentParser name) env of
  Left _ -> error "This indicates a bug in looper because all environment variables are optional."
  Right r -> r

-- | An 'envparse' parser for a 'LooperEnvironment'
looperEnvironmentParser ::
  -- | Name of the looper (best to make this all-caps)
  String ->
  Env.Parser Env.Error LooperEnvironment
looperEnvironmentParser name =
  Env.prefixed (name <> "_") $
    LooperEnvironment
      <$> Env.var (fmap Just . Env.auto) "ENABLED" (Env.def Nothing <> Env.help "Whether to enable this looper")
      <*> Env.var (fmap Just . Env.auto) "PHASE" (Env.def Nothing <> Env.help "The amount of time to wait before starting the looper the first time, in seconds")
      <*> Env.var (fmap Just . Env.auto) "PERIOD" (Env.def Nothing <> Env.help "The amount of time to wait between runs of the looper, in seconds")

-- | A structure to configuration for a looper into
data LooperConfiguration = LooperConfiguration
  { looperConfEnabled :: Maybe Bool,
    looperConfPhase :: Maybe Word,
    looperConfPeriod :: Maybe Word
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec LooperConfiguration)

instance HasCodec LooperConfiguration where
  codec =
    named "LooperConfiguration" $
      object "LooperConfiguration" $
        LooperConfiguration
          <$> parseAlternative
            (optionalFieldOrNull "enable" "Enable this looper")
            (optionalFieldOrNull "enabled" "Enable this looper")
            .= looperConfEnabled
          <*> optionalFieldOrNull "phase" "The amount of time to wait before starting the looper the first time, in seconds" .= looperConfPhase
          <*> optionalFieldOrNull "period" "The amount of time to wait between runs of the looper, in seconds" .= looperConfPeriod

-- | Settings that you might want to pass into a looper using 'mkLooperDef'
data LooperSettings = LooperSettings
  { looperSetEnabled :: Bool,
    looperSetPhase :: NominalDiffTime,
    looperSetPeriod :: NominalDiffTime
  }
  deriving (Show, Eq, Generic)

deriveLooperSettings ::
  -- | Default phase
  NominalDiffTime ->
  -- | Default period
  NominalDiffTime ->
  LooperFlags ->
  LooperEnvironment ->
  Maybe LooperConfiguration ->
  LooperSettings
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
  -- | Name
  Text ->
  LooperSettings ->
  -- | The function to loop
  m () ->
  LooperDef m
mkLooperDef name LooperSettings {..} func =
  LooperDef
    { looperDefName = name,
      looperDefEnabled = looperSetEnabled,
      looperDefPeriod = looperSetPeriod,
      looperDefPhase = looperSetPhase,
      looperDefFunc = func
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
  (MonadUnliftIO m, MonadUnliftIO n) =>
  -- | Custom runner
  (LooperDef m -> n ()) ->
  -- | Loopers
  [LooperDef m] ->
  n ()
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
  (MonadUnliftIO m, MonadUnliftIO n) =>
  -- | Overrun handler
  (LooperDef m -> n ()) ->
  -- | Runner
  (LooperDef m -> n ()) ->
  -- | Loopers
  [LooperDef m] ->
  n ()
runLoopersRaw onOverrun runLooper =
  mapConcurrently_ $ \ld@LooperDef {..} ->
    when looperDefEnabled $ do
      waitNominalDiffTime looperDefPhase
      let loop = do
            start <- liftIO getCurrentTime
            runLooper ld
            end <- liftIO getCurrentTime
            let elapsed = diffUTCTime end start
            let nextWait = looperDefPeriod - elapsed
            if nextWait < 0
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
