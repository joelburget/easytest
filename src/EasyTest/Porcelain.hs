{-# Language OverloadedStrings #-}
module EasyTest.Porcelain where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack
import System.Exit
import qualified Control.Concurrent.Async as A
import qualified Data.Map as Map
import qualified System.Random as Random

import EasyTest.Core

io :: IO a -> Test a
io = liftIO

expect :: HasCallStack => Bool -> Test ()
expect False = crash "unexpected"
expect True = ok

expectJust :: HasCallStack => Maybe a -> Test a
expectJust Nothing = crash "expected Just, got Nothing"
expectJust (Just a) = ok >> pure a

expectRight :: HasCallStack => Either e a -> Test a
expectRight (Left _) = crash "expected Right, got Left"
expectRight (Right a) = ok >> pure a

expectEq :: (Eq a, Show a, HasCallStack) => a -> a -> Test ()
expectEq x y = if x == y then ok else crash $
  "expected to be equal: (" <> show' x <> "), (" <> show' y <> ")"

tests :: [Test ()] -> Test ()
tests = msum

-- | Run all tests whose scope starts with the given prefix
runOnly :: Text -> Test a -> IO ()
runOnly prefix t = do
  logger <- atomicLogger
  seed <- abs <$> Random.randomIO :: IO Int
  let allow = filter (not . T.null) $ T.splitOn "." prefix
  run' seed logger allow t

-- | Run all tests with the given seed and whose scope starts with the given prefix
rerunOnly :: Int -> Text -> Test a -> IO ()
rerunOnly seed prefix t = do
  logger <- atomicLogger
  let allow = filter (not . T.null) $ T.splitOn "." prefix
  run' seed logger allow t

run :: Test a -> IO ()
run = runOnly ""

rerun :: Int -> Test a -> IO ()
rerun seed = rerunOnly seed ""

run' :: Int -> (Text -> IO ()) -> [Text] -> Test a -> IO ()
run' seed note allow (Test t) = do
  let !rng = Random.mkStdGen seed
  resultsQ <- atomically (newTBQueue 50)
  rngVar <- newTVarIO rng
  note $ "Randomness seed for this run is " <> show' seed <> ""
  results <- atomically $ newTVar Map.empty
  rs <- A.async . forever $ do
    -- note, totally fine if this bombs once queue is empty
    Just result <- atomically $ readTBQueue resultsQ
    (msgs, passed) <- atomically $ takeTMVar result
    let msgs' = T.intercalate "." msgs
    atomically $ modifyTVar results (Map.insertWith combineStatus msgs' passed)
    resultsMap <- readTVarIO results
    case Map.findWithDefault Skipped msgs' resultsMap of
      Skipped -> pure ()
      Passed n -> note $ "OK " <> (if n <= 1 then msgs' else "(" <> show' n <> ") " <> msgs')
      Failed -> note $ "FAILED " <> msgs'
  let line = "------------------------------------------------------------"
  note "Raw test output to follow ... "
  note line
  result <- try (runReaderT (void t) (Env rngVar [] resultsQ note allow))
    :: IO (Either SomeException ())
  case result of
    Left e -> note $ "Exception while running tests: " <> show' e
    Right () -> pure ()
  atomically $ writeTBQueue resultsQ Nothing
  _ <- A.waitCatch rs
  resultsMap <- readTVarIO results
  let
    resultsList = Map.toList resultsMap
    succeededList = [ n | (_, Passed n) <- resultsList ]
    succeeded = length succeededList
    -- totalTestCases = foldl' (+) 0 succeededList
    failures = [ a | (a, Failed) <- resultsList ]
    failed = length failures
  case failures of
    [] -> do
      note line
      case succeeded of
        0 -> do
          note "😶  hmm ... no test results recorded"
          note "Tip: use `ok`, `expect`, or `crash` to record results"
          note "Tip: if running via `runOnly` or `rerunOnly`, check for typos"
        1 -> note   "✅  1 test passed, no failures! 👍 🎉"
        _ -> note $ "✅  " <> show' succeeded <> " tests passed, no failures! 👍 🎉"
    (hd:_) -> do
      note line
      note "\n"
      note $ "  " <> show' succeeded <> (if failed == 0 then " PASSED" else " passed")
      note $ "  " <> show' (length failures) <> (if failed == 0 then " failed" else " FAILED (failed scopes below)")
      note $ "    " <> T.intercalate "\n    " (map show' failures)
      note ""
      note   "  To rerun with same random seed:\n"
      note $ "    EasyTest.rerun " <> show' seed
      note $ "    EasyTest.rerunOnly " <> show' seed <> " " <> "\"" <> hd <> "\""
      note "\n"
      note line
      note "❌"
      exitWith (ExitFailure 1)

-- | Label a test. Can be nested. A `'.'` is placed between nested
-- scopes, so `scope "foo" . scope "bar"` is equivalent to `scope "foo.bar"`
scope :: Text -> Test a -> Test a
scope msg (Test t) = Test $ do
  env <- ask
  let msg' = T.splitOn "." msg
      messages' = messages env <> msg'
      env' = env { messages = messages' }
      passes = actionAllowed env'

  if passes
    then liftIO $ runReaderT t env'
    else putResult Skipped >> pure Nothing

-- | Log a showable value
note' :: Show s => s -> Test ()
note' = note . show'

-- | Record a successful test at the current scope
ok :: Test ()
ok = Test (Just <$> putResult (Passed 1))

-- | Explicitly skip this test
skip :: Test ()
skip = Test (Nothing <$ putResult Skipped)

-- | Run a test in a separate thread, not blocking for its result.
fork :: Test a -> Test ()
fork t = void (fork' t)

-- | Run a test in a separate thread, return a future which can be used
-- to block on its result.
fork' :: Test a -> Test (Test a)
fork' (Test t) = do
  env <- ask
  tmvar <- liftIO newEmptyTMVarIO
  liftIO . atomically $ writeTBQueue (results env) (Just tmvar)
  r <- liftIO . A.async $ runWrap env t
  waiter <- liftIO . A.async $ do
    e <- A.waitCatch r
    _ <- atomically $ tryPutTMVar tmvar (messages env, Skipped)
    case e of
      Left _ -> pure Nothing
      Right a -> pure a
  pure $ do
    a <- liftIO (A.wait waiter)
    case a of Nothing -> empty
              Just a' -> pure a'
