{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Timeout
import Looper
import Weigh

main :: IO ()
main = Weigh.mainWith $ do
  action "empty" $ timeout 1 $ runLoopers []
  let waitLooper =
        mkLooperDef
          "wait"
          ( LooperSettings
              { looperSetEnabled = True,
                -- Start immediately
                looperSetPhase = 0,
                -- Every 100 ms
                looperSetPeriod = milliseconds 100
              }
          )
          -- 50 milliseconds
          (threadDelay 50_000)
  action "wait 50 milliseconds, 1 sec" $ timeout 1 $ runLoopers [waitLooper]
  action "wait 50 milliseconds, 2 sec" $ timeout 2 $ runLoopers [waitLooper]
  action "wait 50 milliseconds, 4 sec" $ timeout 4 $ runLoopers [waitLooper]
  action "wait 50 milliseconds, 8 sec" $ timeout 8 $ runLoopers [waitLooper]
