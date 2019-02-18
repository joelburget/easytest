{-# LANGUAGE CPP #-}
module EasyTest.Hedgehog where

import           Control.Monad.IO.Class

import           Control.Concurrent.STM.TVar
  (TVar, modifyTVar', newTVar, readTVar)
import           Control.Monad.STM           (atomically)
import           Hedgehog                    hiding (Test)
import           Hedgehog.Internal.Config
import           Hedgehog.Internal.Property
import           Hedgehog.Internal.Queue
import           Hedgehog.Internal.Region
import           Hedgehog.Internal.Report
import           Hedgehog.Internal.Runner    hiding (checkNamed)

-- | 'Hedgehog.Internal.Runner.checkNamed' modified to take a 'Seed'
checkNamed ::
     MonadIO m
  => Region
  -> Maybe UseColor
  -> Maybe PropertyName
  -> Seed
  -> Property
  -> m (Report Result)
checkNamed region mcolor name seed prop
  = checkRegion region mcolor name 0 seed prop

-- | 'Hedgehog.Internal.Runner.updateSummary' exposed.
updateSummary :: Region -> TVar Summary -> Maybe UseColor -> (Summary -> Summary) -> IO ()
updateSummary sregion svar mcolor f = do
  summary <- atomically (modifyTVar' svar f >> readTVar svar)
  setRegion sregion =<< renderSummary mcolor summary

-- | 'Hedgehog.Internal.Runner.checkGroupWith' modified to take a 'Seed'
checkGroupWith ::
     WorkerCount
  -> Verbosity
  -> Maybe UseColor
  -> Seed
  -> [(PropertyName, Property)]
  -> IO Summary
checkGroupWith n verbosity mcolor seed props =
  displayRegion $ \sregion -> do
    svar <- atomically . newTVar $ mempty { summaryWaiting = PropertyCount (length props) }

    let
      start (TasksRemaining tasks) _ix (name, prop) =
        liftIO $ do
          updateSummary sregion svar mcolor $ \x -> x {
              summaryWaiting =
                PropertyCount tasks
            , summaryRunning =
                summaryRunning x + 1
            }

          atomically $ do
            region <-
              case verbosity of
                Quiet ->
                  newEmptyRegion
                Normal ->
                  newOpenRegion

            moveToBottom sregion

            pure (name, prop, region)

      finish (_name, _prop, _region) =
        updateSummary sregion svar mcolor $ \x -> x {
            summaryRunning =
              summaryRunning x - 1
          }

      finalize (_name, _prop, region) =
        finishRegion region

    summary <-
      fmap (mconcat . fmap (fromResult . reportStatus)) $
        runTasks n props start finish finalize $ \(name, prop, region) -> do
          result <- checkNamed region mcolor (Just name) seed prop
          updateSummary sregion svar mcolor
            (<> fromResult (reportStatus result))
          pure result

    updateSummary sregion svar mcolor (const summary)
    pure summary

-- 'checkSequential' modified to take a seed
recheck' :: MonadIO m => Seed -> Group -> m Bool
recheck' seed (Group group props) = liftIO $ do
  let config = RunnerConfig {
        runnerWorkers =
          Just 1
      , runnerColor =
          Nothing
      , runnerVerbosity =
          Nothing
      }
  n <- resolveWorkers (runnerWorkers config)

  -- ensure few spare capabilities for concurrent-output, it's likely that
  -- our tests will saturate all the capabilities they're given.
  updateNumCapabilities (n + 2)

#if mingw32_HOST_OS
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
#endif
  putStrLn $ "━━━ " ++ unGroupName group ++ " ━━━"

  verbosity <- resolveVerbosity (runnerVerbosity config)
  summary <- checkGroupWith n verbosity (runnerColor config) seed props

  pure $
    summaryFailed summary == 0 &&
    summaryGaveUp summary == 0
