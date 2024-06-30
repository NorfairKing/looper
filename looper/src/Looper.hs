{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Looper
  ( LooperDef (..),
    milliseconds,
    seconds,
    minutes,
    hours,
    LooperSettings (..),
    parseLooperSettings,
    mkLooperDef,
    runLoopers,
    runLoopersIgnoreOverrun,
    runLoopersRaw,
    waitNominalDiffTime,
  )
where

import Control.Monad
import Data.Text (Text)
import Data.Time
import GHC.Generics (Generic)
import OptEnvConf
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

-- | Construct a 'NominalDiffTime' from a number of milliseconds
--
-- Note that scheduling can easily get in the way of accuracy at this
-- level of granularity.
milliseconds :: Double -> NominalDiffTime
milliseconds = seconds . (/ 60)

-- | Construct a 'NominalDiffTime' from a number of seconds
seconds :: Double -> NominalDiffTime
seconds = realToFrac

-- | Construct a 'NominalDiffTime' from a number of minutes
minutes :: Double -> NominalDiffTime
minutes = seconds . (* 60)

-- | Construct a 'NominalDiffTime' from a number of hours
hours :: Double -> NominalDiffTime
hours = minutes . (* 60)

-- | Settings that you might want to pass into a looper using 'mkLooperDef'
data LooperSettings = LooperSettings
  { looperSetEnabled :: Bool,
    looperSetPhase :: NominalDiffTime,
    looperSetPeriod :: NominalDiffTime
  }
  deriving (Show, Eq, Generic)

parseLooperSettings :: String -> Parser LooperSettings
parseLooperSettings looperName = do
  looperSetEnabled <-
    subConfig (toConfigCase looperName) $
      subEnv (toEnvCase looperName <> "_") $
        enableDisableSwitch
          True
          [ help $ unwords ["enable the", looperName, "looper"],
            option,
            long looperName,
            env "ENABLE",
            conf "enable"
          ]
  (looperSetPhase, looperSetPeriod) <- subAll looperName $ do
    ph <-
      setting
        [ help $ unwords ["phase of the", looperName, "looper in seconds"],
          reader auto,
          option,
          name "phase",
          metavar "SECONDS"
        ]
    pe <-
      setting
        [ help $ unwords ["period of the", looperName, "looper in seconds"],
          reader auto,
          name "period",
          metavar "SECONDS"
        ]
    pure (ph, pe)
  pure LooperSettings {..}

mkLooperDef ::
  -- | Name
  Text ->
  LooperSettings ->
  -- | The function to loop
  m () ->
  LooperDef m
mkLooperDef n LooperSettings {..} func =
  LooperDef
    { looperDefName = n,
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
runLoopers :: (MonadUnliftIO m) => [LooperDef m] -> m ()
runLoopers = runLoopersIgnoreOverrun looperDefFunc

-- | Run loopers with a custom runner, ignoring any overruns
--
-- > runLoopersIgnoreOverrun = runLoopersRaw (pure ())
--
-- see 'runLoopersRaw'
--
-- Note that this function will loop forever, you need to wrap it using 'async' yourself.
runLoopersIgnoreOverrun ::
  (MonadUnliftIO n) =>
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
  (MonadUnliftIO n) =>
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
waitNominalDiffTime :: (MonadIO m) => NominalDiffTime -> m ()
waitNominalDiffTime ndt = liftIO $ threadDelay $ round (toRational ndt * 1_000_000)
