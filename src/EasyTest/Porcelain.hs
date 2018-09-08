{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , expectNeq
  , tests
  -- , using
  , runOnly
  -- , rerunOnly
  , run
  -- , rerun
  , scope
  , note'
  , ok
  , skip
  , crash
  , note
  ) where

import           Control.Monad.Reader
import           Control.Monad.Trans.Writer.Strict (runWriter)
import           Control.Monad.Writer.Class (tell)
import           Data.List (isPrefixOf)
#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup
#endif
import Data.String (fromString)
import           Data.CallStack

import           EasyTest.Internal

import Hedgehog hiding (Test)
import Hedgehog.Internal.Property (PropertyName(PropertyName))


expect :: (HasCallStack) => Bool -> Test ()
expect False = crash "unexpected"
expect True  = ok

expectJust :: (HasCallStack) => Maybe a -> Test ()
expectJust Nothing  = crash "expected Just, got Nothing"
expectJust (Just _) = ok

expectRight :: (Show e, HasCallStack) => Either e a -> Test ()
expectRight (Left e)  = crash $ "expected Right, got (Left " ++ show e ++ ")"
expectRight (Right _) = ok

expectRightNoShow :: (HasCallStack) => Either e a -> Test ()
expectRightNoShow (Left _)  = crash $ "expected Right, got Left"
expectRightNoShow (Right _) = ok

expectLeft :: (Show a, HasCallStack) => Either e a -> Test ()
expectLeft (Right a) = crash $ "expected Left, got (Right " ++ show a ++ ")"
expectLeft (Left _)  = ok

expectLeftNoShow :: (HasCallStack) => Either e a -> Test ()
expectLeftNoShow (Right _) = crash $ "expected Left, got Right"
expectLeftNoShow (Left _)  = ok

expectEq :: (Eq a, Show a, HasCallStack) => a -> a -> Test ()
expectEq a b = do
  name <- getPropertyName
  tell [(name, property' $ a === b)]

expectNeq :: (Eq a, Show a, HasCallStack) => a -> a -> Test ()
expectNeq a b = do
  name <- getPropertyName
  tell [(name, property' $ a /== b)]

-- | Run a list of tests
--
-- This specializes 'sequence_'.
tests :: [Test ()] -> Test ()
tests = sequence_

---- | A test with a setup and teardown
--using :: IO r -> (r -> IO ()) -> (r -> Test a) -> Test a
--using r cleanup use = Test $ MaybeT $ do
--  r' <- liftIO r
--  env <- ask
--  let Test t = use r'
--  a <- liftIO (runWrap env t)
--  liftIO (cleanup r')
--  pure a

---- | Run all tests whose scope starts with the given prefix
-- runOnly :: Text -> Test a -> IO ()
runOnly :: String -> Test () -> IO ()
runOnly prefix t = do
  let ((), props) = runWriter $ runReaderT (unTest t) (Env [])
      props' = filter (\(PropertyName name, _) -> prefix `isPrefixOf` name)
        props
      -- props' = flip fmap props $ \(pname@(PropertyName name), prop) ->
      --   case prefix `isPrefixOf` name of
      --     False -> (pname, property' $ do { note "skipped"; success; })
      --     True  -> (pname, prop)
      -- TODO: show skipped
      group = Group (fromString $ "runOnly " ++ show prefix) props'
  void $ checkSequential group

---- | Rerun all tests with the given seed and whose scope starts with the given prefix
--rerunOnly :: Int -> Text -> Test a -> IO ()
--rerunOnly seed prefix t = do
--  (msgLogger, diffLogger) <- atomicLogger
--  let allowed = filter (not . T.null) $ T.splitOn "." prefix
--  run' seed msgLogger diffLogger allowed t

-- | Run all tests
run :: Test () -> IO ()
run t =  -- runOnly ""
  let ((), props) = runWriter $ runReaderT (unTest t) (Env [])
      group = Group "run" props
  in void $ checkSequential group

-- | Rerun all tests with the given seed
-- rerun :: Size -> Seed -> Test () -> IO ()
-- rerun size seed t = -- rerunOnly seed ""
--   let ((), props) = runWriter $ runReaderT (unTest t) (Env [])
--   in recheck size seed props

-- | Log a string
note :: MonadTest m => String -> m ()
note = footnote

-- | Log a showable value
note' :: (MonadTest m, Show s) => s -> m ()
note' = footnoteShow

---- | Record a successful test at the current scope
ok :: Test ()
ok = do
  name <- getPropertyName
  testProperty $ property' success

-- | Explicitly skip this test
skip :: Test ()
skip = do
  name <- getPropertyName
  testProperty $ property' discard
