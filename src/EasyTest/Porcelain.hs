{-# language BangPatterns #-}
{-# language CPP #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}

module EasyTest.Porcelain
  ( -- * Tests
    Test
  , expect
  , expectJust
  , expectRight
  , expectRightNoShow
  , expectLeft
  , expectLeftNoShow
  , expectEq
  , tests
  , using
  , runOnly
  , rerunOnly
  , run
  , rerun
  , scope
  , note'
  , ok
  , skip
  , fork
  , fork'
  , crash
  , note
  , io
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.CallStack
import System.Exit
import System.IO
import qualified Control.Concurrent.Async as A
import qualified Data.Map as Map
import qualified System.Random as Random

import EasyTest.Internal

-- | Convenient alias for 'liftIO'
io :: IO a -> Test a
io = liftIO

expect :: HasCallStack => Bool -> Test ()
expect False = crash "unexpected"
expect True = ok

expectJust :: HasCallStack => Maybe a -> Test ()
expectJust Nothing = crash "expected Just, got Nothing"
expectJust (Just _) = ok

expectRight :: (Show e, HasCallStack) => Either e a -> Test ()
expectRight (Left e)  = crash $ "expected Right, got (Left " <> T.pack (show e) <> ")"
expectRight (Right _) = ok

expectRightNoShow :: HasCallStack => Either e a -> Test ()
expectRightNoShow (Left _)  = crash $ "expected Right, got Left"
expectRightNoShow (Right _) = ok

expectLeft :: (Show a, HasCallStack) => Either e a -> Test ()
expectLeft (Right a) = crash $ "expected Left, got (Right " <> T.pack (show a) <> ")"
expectLeft (Left _)  = ok

expectLeftNoShow :: HasCallStack => Either e a -> Test ()
expectLeftNoShow (Right _) = crash $ "expected Left, got Right"
expectLeftNoShow (Left _)  = ok

expectEq :: (Eq a, Show a, HasCallStack) => a -> a -> Test ()
expectEq x y = if x == y then ok else crash $
  "expected to be equal: (" <> show' x <> "), (" <> show' y <> ")"

-- | Run a list of tests
--
-- This specializes 'msum', 'Data.Foldable.asum', and 'sequence_'.
tests :: [Test ()] -> Test ()
tests = msum

atomicLogger :: IO (Text -> IO ())
atomicLogger = do
  lock <- newMVar ()
  pure $ \msg ->
    -- force msg before acquiring lock
    let dummy = T.foldl' (\_ ch -> ch == 'a') True msg
    in dummy `seq` bracket (takeMVar lock) (\_ -> putMVar lock ()) (\_ -> T.putStrLn $ sanitize msg)

sanitize :: Text -> Text
sanitize msg = if isUnicodeLocale
    then msg
    else T.replace "✅" "!"
       . T.replace "❌" "X"
       . T.replace "😶" ":/"
       . T.replace "👍" ":D"
       . T.replace "🎉" ":P"
       $ msg

isUnicodeLocale :: Bool
isUnicodeLocale = elem (show localeEncoding) $ map show [utf8, utf8_bom, utf16, utf16le, utf16be, utf32, utf32le, utf32be]

-- | A test with a setup and teardown
using :: IO r -> (r -> IO ()) -> (r -> Test a) -> Test a
using r cleanup use = Test $ do
  r' <- liftIO r
  env <- ask
  let Test t = use r'
  a <- liftIO (runWrap env t)
  liftIO (cleanup r')
  pure a

-- | Run all tests whose scope starts with the given prefix
runOnly :: Text -> Test a -> IO ()
runOnly prefix t = do
  logger <- atomicLogger
  seed <- abs <$> Random.randomIO :: IO Int
  let allowed = filter (not . T.null) $ T.splitOn "." prefix
  run' seed logger allowed t

-- | Rerun all tests with the given seed and whose scope starts with the given prefix
rerunOnly :: Int -> Text -> Test a -> IO ()
rerunOnly seed prefix t = do
  logger <- atomicLogger
  let allowed = filter (not . T.null) $ T.splitOn "." prefix
  run' seed logger allowed t

-- | Run all tests
run :: Test a -> IO ()
run = runOnly ""

-- | Rerun all tests with the given seed
rerun :: Int -> Test a -> IO ()
rerun seed = rerunOnly seed ""

run' :: Int -> (Text -> IO ()) -> [Text] -> Test a -> IO ()
run' seed note_ allowed (Test t) = do
  let !rng_ = Random.mkStdGen seed
  resultsQ <- atomically (newTBQueue 50)
  rngVar <- newTVarIO rng_
  note_ $ "Randomness seed for this run is " <> show' seed <> ""
  results <- atomically $ newTVar Map.empty
  rs <- A.async . forever $ do
    -- note, totally fine if this bombs once queue is empty
    Just result <- atomically $ readTBQueue resultsQ
    (msgs, passed) <- atomically $ takeTMVar result
    let msgs' = T.intercalate "." msgs
    atomically $ modifyTVar results (Map.insertWith combineStatus msgs' passed)
    resultsMap <- readTVarIO results
    case Map.findWithDefault Skipped msgs' resultsMap of
      Skipped  -> pure ()
      Passed n -> note_ $ "OK " <> (if n <= 1 then msgs' else "(" <> show' n <> ") " <> msgs')
      Failed   -> note_ $ "FAILED " <> msgs'
  let line = "------------------------------------------------------------"
  note_ "Raw test output to follow ... "
  note_ line
  result <- try (runReaderT (void t) (Env rngVar [] resultsQ note_ allowed))
    :: IO (Either SomeException ())
  case result of
    Left e -> note_ $ "Exception while running tests: " <> show' e
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
      note_ line
      case succeeded of
        0 -> do
          note_ $ T.unlines
            [ "😶  hmm ... no test results recorded"
            , "Tip: use `ok`, `expect`, or `crash` to record results"
            , "Tip: if running via `runOnly` or `rerunOnly`, check for typos"
            ]
        1 -> note_   "✅  1 test passed, no failures! 👍 🎉"
        _ -> note_ $ "✅  " <> show' succeeded <> " tests passed, no failures! 👍 🎉"
    hd:_ -> do
      note_ $ T.unlines
        [ line
        , "\n"
        , "  " <> show' succeeded <> (if failed == 0 then " PASSED" else " passed")
        , "  " <> show' (length failures) <> (if failed == 0 then " failed" else " FAILED (failed scopes below)")
        , "    " <> T.intercalate "\n    " (map show' failures)
        , ""
        , "  To rerun with same random seed:\n"
        , "    EasyTest.rerun " <> show' seed
        , "    EasyTest.rerunOnly " <> show' seed <> " " <> "\"" <> hd <> "\""
        , "\n"
        , line
        , "❌"
        ]
      exitWith (ExitFailure 1)

-- TODO: replace with show-text?
show' :: Show a => a -> Text
show' = T.pack . show

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
  liftIO . atomically $ writeTBQueue (envResults env) (Just tmvar)
  r <- liftIO . A.async $ runWrap env t
  waiter <- liftIO . A.async $ do
    e <- A.waitCatch r
    _ <- atomically $ tryPutTMVar tmvar (envMessages env, Skipped)
    case e of
      Left _ -> pure Nothing
      Right a -> pure a
  pure $ do
    a <- liftIO (A.wait waiter)
    case a of Nothing -> empty
              Just a' -> pure a'
