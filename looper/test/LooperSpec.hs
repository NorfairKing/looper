{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module LooperSpec
  ( spec,
  )
where

import Autodocodec.Yaml
import Looper
import Options.Applicative as AP
import Test.Syd
import UnliftIO

spec :: Spec
spec = do
  describe "getLooperFlags" $ do
    it "parses default values for an empty list of arguments" $ do
      parserSucceedsWith
        (getLooperFlags "test")
        []
        ( LooperFlags
            { looperFlagEnabled = Nothing,
              looperFlagPhase = Nothing,
              looperFlagPeriod = Nothing
            }
        )
    it "parses an enable flag correctly" $ do
      parserSucceedsWith
        (getLooperFlags "test")
        ["--enable-test"]
        ( LooperFlags
            { looperFlagEnabled = Just True,
              looperFlagPhase = Nothing,
              looperFlagPeriod = Nothing
            }
        )
    it "parses an disable flag correctly" $ do
      parserSucceedsWith
        (getLooperFlags "test")
        ["--disable-test"]
        ( LooperFlags
            { looperFlagEnabled = Just False,
              looperFlagPhase = Nothing,
              looperFlagPeriod = Nothing
            }
        )

  describe "Configuration" $ do
    it "has the same schema as before" $
      pureGoldenTextFile "test_resources/configuration.txt" (renderColouredSchemaViaCodec @LooperConfiguration)

  describe "runLoopers" $ do
    it "runs one looper as intended" $ do
      v <- newTVarIO (0 :: Int)
      let l =
            LooperDef
              { looperDefName = "l1",
                looperDefEnabled = True,
                looperDefPeriod = seconds 1,
                looperDefPhase = seconds 0,
                looperDefFunc = atomically $ modifyTVar' v succ
              }
      a <- async $ runLoopers [l]
      waitNominalDiffTime $ seconds 1.5
      cancel a
      r <- readTVarIO v
      r `shouldBe` 2
    it "does not run a looper before its phase" $ do
      v <- newTVarIO (0 :: Int)
      let l =
            LooperDef
              { looperDefName = "l",
                looperDefEnabled = True,
                looperDefPeriod = seconds 1,
                looperDefPhase = seconds 2,
                looperDefFunc = atomically $ modifyTVar' v succ
              }
      a <- async $ runLoopers [l]
      waitNominalDiffTime $ seconds 1.5
      cancel a
      r <- readTVarIO v
      r `shouldBe` 0
    it "runs two loopers as intended" $ do
      v1 <- newTVarIO (0 :: Int)
      v2 <- newTVarIO (0 :: Int)
      v3 <- newTVarIO (0 :: Int)
      let l1 =
            LooperDef
              { looperDefName = "l1",
                looperDefEnabled = True,
                looperDefPeriod = seconds 1,
                looperDefPhase = seconds 0,
                looperDefFunc =
                  atomically $ do
                    modifyTVar' v1 succ
                    modifyTVar' v2 succ
              }
      let l2 =
            LooperDef
              { looperDefName = "l2",
                looperDefEnabled = True,
                looperDefPeriod = seconds 0.5,
                looperDefPhase = seconds 0.5,
                looperDefFunc =
                  atomically $ do
                    modifyTVar' v2 succ
                    modifyTVar' v3 succ
              }
      a <- async $ runLoopers [l1, l2]
      waitNominalDiffTime $ seconds 2.25
      cancel a
      r1 <- readTVarIO v1
      r2 <- readTVarIO v2
      r3 <- readTVarIO v3
      (r1, r2, r3) `shouldBe` (3, 7, 4)
  describe "runLoopersRaw" $ do
    it "runs one looper as intended with a custom runner" $ do
      v1 <- newTVarIO (0 :: Int)
      v2 <- newTVarIO (0 :: Int)
      let inc1 = atomically $ modifyTVar' v1 succ
          inc2 = atomically $ modifyTVar' v2 succ
      let l =
            LooperDef
              { looperDefName = "l",
                looperDefEnabled = True,
                looperDefPeriod = seconds 1,
                looperDefPhase = seconds 0,
                looperDefFunc = inc1
              }
      a <- async $ runLoopersRaw (const $ pure ()) (\ld -> looperDefFunc ld >> inc2) [l]
      waitNominalDiffTime $ seconds 1.5
      cancel a
      r1 <- readTVarIO v1
      r2 <- readTVarIO v2
      (r1, r2) `shouldBe` (2, 2)
    it "runs one looper as intended with a custom overrun hook" $ do
      v1 <- newTVarIO (0 :: Int)
      v2 <- newTVarIO (0 :: Int)
      let inc1 = atomically $ modifyTVar' v1 succ
          inc2 = atomically $ modifyTVar' v2 succ
      let l =
            LooperDef
              { looperDefName = "l",
                looperDefEnabled = True,
                looperDefPeriod = seconds 1,
                looperDefPhase = seconds 0,
                looperDefFunc = inc1 >> waitNominalDiffTime (seconds 1.5)
              }
      a <- async $ runLoopersRaw (const inc2) looperDefFunc [l]
      waitNominalDiffTime $ seconds 3.5
      cancel a
      r1 <- readTVarIO v1
      r2 <- readTVarIO v2
      (r1, r2) `shouldBe` (3, 2)

parserSucceedsWith :: (Show a, Eq a) => Parser a -> [String] -> a -> Expectation
parserSucceedsWith parser args expectedValue =
  case execParserPure parserPrefs (info parser mempty) args of
    AP.Success r -> r `shouldBe` expectedValue
    AP.Failure fp ->
      let (err, ec) = renderFailure fp "test"
       in expectationFailure $
            unlines ["Failed to parse:", err, "would have resulted in exit code", show ec]
    AP.CompletionInvoked _ -> expectationFailure "Tried to invoke a completion, should not happen"
  where
    parserPrefs :: ParserPrefs
    parserPrefs =
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
        }
