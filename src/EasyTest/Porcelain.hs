{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : EasyTest.Porcelain
-- Copyright   : (c) Joel Burget, 2018-2019
-- License     : MIT
-- Maintainer  : joelburget@gmail.com
-- Stability   : experimental
--
-- This module defines the interface for EasyTest.
module EasyTest.Porcelain
  ( -- * Assertions
    expect
  , expectJust
  , expectRight
  , expectRightNoShow
  , expectLeft
  , expectLeftNoShow
  , expectEq
  , expectNeq
  , ok
  , skip
  , crash
  -- * Structuring tests
  , tests
  , scope
  , testProperty
  , unitTest
  -- * Running tests
  , runOnly
  , rerunOnly
  , run
  , rerun
  -- * Notes
  , noteShow
  , note
  ) where

import           Control.Monad     (void)
import           Data.List         (intercalate)
#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup
#endif
import           Data.CallStack
import           Data.List.Split   (splitOn)
import           Data.String       (fromString)

import           Hedgehog          hiding (Test)
import           Hedgehog.Internal.Seed (random)

import           EasyTest.Hedgehog
import           EasyTest.Internal


-- | Examples:
--
-- >>> run $ unitTest $ 1 === 2
-- > ━━━ run ━━━
-- >   ✗ (unnamed) failed after 1 test.
-- >
-- >        ┏━━ tests/Suite.hs ━━━
-- >     26 ┃ main :: IO ()
-- >     27 ┃ main = do
-- >     28 ┃   run $ unitTest $ 1 === (2 :: Int)
-- >        ┃   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- >        ┃   │ Failed (- lhs =/= + rhs)
-- >        ┃   │ - 1
-- >        ┃   │ + 2
-- >
-- >     This failure can be reproduced by running:
-- >     > recheck (Size 0) (Seed 2914818620245020776 12314041441884757111) (unnamed)
-- >
-- >   ✗ 1 failed.
unitTest :: HasCallStack => PropertyT IO () -> Test
unitTest = testProperty . unitProperty

expect :: HasCallStack => Bool -> Test
expect False = crash "unexpected"
expect True  = ok

expectJust :: HasCallStack => Maybe a -> Test
expectJust Nothing  = crash "expected Just, got Nothing"
expectJust (Just _) = ok

expectRight :: (Show e, HasCallStack) => Either e a -> Test
expectRight (Left e)  = crash $ "expected Right, got (Left " ++ show e ++ ")"
expectRight (Right _) = ok

expectRightNoShow :: (HasCallStack) => Either e a -> Test
expectRightNoShow (Left _)  = crash $ "expected Right, got Left"
expectRightNoShow (Right _) = ok

expectLeft :: (Show a, HasCallStack) => Either e a -> Test
expectLeft (Right a) = crash $ "expected Left, got (Right " ++ show a ++ ")"
expectLeft (Left _)  = ok

expectLeftNoShow :: HasCallStack => Either e a -> Test
expectLeftNoShow (Right _) = crash $ "expected Left, got Right"
expectLeftNoShow (Left _)  = ok

expectEq :: (Eq a, Show a, HasCallStack) => a -> a -> Test
expectEq a b = unitTest $ a === b

expectNeq :: (Eq a, Show a, HasCallStack) => a -> a -> Test
expectNeq a b = unitTest $ a === b

-- | Run a list of tests
tests :: [Test] -> Test
tests = Sequence

-- | Make a 'Hedgehog.Group' from a list of tests.
mkGroup :: GroupName -> [([String], Property)] -> Group
mkGroup name props = Group name $ flip fmap props $ \(path, prop) ->
  case path of
    [] -> ("(unnamed)", prop)
    _  -> (fromString (intercalate "." path), prop)

-- | Flatten a test tree. Use with 'mkGroup'
runTree :: Test -> [([String], Property)]
runTree = runTree' []

runTree' :: [String] -> Test -> [([String], Property)]
runTree' stack = \case
  Leaf prop      -> [(reverse stack, prop)]
  Sequence trees -> concatMap go $ zip seqNames trees
  Internal trees -> concatMap go trees
  where go (name, tree) = runTree' (name:stack) tree

-- | Flatten a subtree of tests. Use with 'mkGroup'
runTreeOnly :: [String] -> Test -> [([String], Property)]
runTreeOnly = runTreeOnly' [] where

  runTreeOnly' stack []              tree             = runTree' stack tree
  runTreeOnly' stack (_:_)           tree@Leaf{}      = skipTree' stack tree
  runTreeOnly' stack scopes          (Sequence nodes)
    = concatMap go $ zip seqNames nodes
    where go (name, tree) = runTreeOnly' (name:stack) scopes tree
  runTreeOnly' stack (scope':scopes) (Internal nodes)
    = concatMap go nodes
    where go (name, tree) =
            if name == scope'
            then runTreeOnly' (name:stack) scopes tree
            else skipTree'    (name:stack)        tree

-- | Skip this test tree (mark all properties as skipped).
skipTree' :: [String] -> Test -> [([String], Property)]
skipTree' stack = \case
  Leaf _prop     -> [(reverse stack, unitProperty discard)]
  Sequence trees -> concatMap go $ zip seqNames trees
  Internal trees -> concatMap go trees

  where go (name, tree) = skipTree' (name:stack) tree

seqNames :: [String]
seqNames = map (\i -> "(" ++ show i ++ ")") [(1 :: Int)..]

-- | Run all tests whose scope starts with the given prefix
runOnly :: String -> Test -> IO ()
runOnly prefix t = do
  let props = runTreeOnly (splitOn "." prefix) t
      group = mkGroup (fromString $ "runOnly " ++ show prefix) props

  seed <- random
  void $ recheckSeed seed group

-- | Rerun all tests with the given seed and whose scope starts with the given
-- prefix
rerunOnly :: String -> Seed -> Test -> IO Bool
rerunOnly prefix seed t = do
  let props = runTreeOnly (splitOn "." prefix) t
      name = fromString $ "rerunOnly " ++ show prefix
  recheckSeed seed $ mkGroup name props

-- | Run all tests
run :: Test -> IO ()
run t = do
  seed <- random
  void $ recheckSeed seed $ mkGroup "run" $ runTree t

-- | Rerun all tests with the given seed
rerun :: Seed -> Test -> IO Bool
rerun = rerunOnly ""

-- | Log a string
note :: MonadTest m => String -> m ()
note = footnote

-- | Log a showable value
noteShow :: (MonadTest m, Show s) => s -> m ()
noteShow = footnoteShow

-- | Record a successful test at the current scope
ok :: Test
ok = unitTest success

-- | Explicitly skip this test
skip :: Test
skip = unitTest discard
