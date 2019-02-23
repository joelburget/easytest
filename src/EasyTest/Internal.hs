{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : EasyTest.Internal
-- Copyright   : (c) Joel Burget, 2018-2019
-- License     : MIT
-- Maintainer  : joelburget@gmail.com
-- Stability   : experimental
--
-- This module defines the core internals and interface of easytest.
module EasyTest.Internal
  (
  -- * Structuring tests
    tests
  , scope
  , unitTest
  , propertyTest
  -- * Running tests
  , run
  , runOnly
  , rerun
  , rerunOnly
  -- -- * Notes
  -- , note
  -- , noteShow
  -- * Assertions
  , expect
  , expectJust
  , expectRight
  , expectRightNoShow
  , expectLeft
  , expectLeftNoShow
  , expectEq
  , expectNeq
  , ok
  , skip
  , pending
  , crash
  -- * Internal
  , Test(..)
  , unitProperty
  , mkTest
  -- * Other
  , io
  -- * Hedgehog re-exports
  , Property
  , PropertyT
  , MonadTest
  , (===)
  , (/==)
  , Seed
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.List              (intercalate)
import           Data.String            (fromString)
#if MIN_VERSION_base(4,9,0)
import           GHC.Stack
#else
import           Data.CallStack
#endif
import           Data.List.Split        (splitOn)

import           Hedgehog               hiding (Test, test)
import           Hedgehog.Internal.Seed (random)

import           EasyTest.Hedgehog


-- | A set of unit- and property-tests
data Test
  = Internal ![(String, Test)]
  -- ^ A set of named (scoped) tests
  | Sequence ![Test]
  -- ^ A sequence of tests
  | Leaf !Property
  -- ^ A single property to check
  | Skipped !Test
  -- ^ A set of tests marked to skip

-- | Make a "unit property" that has no random inputs. IE the special case of a
-- property test that is a unit test.
unitProperty :: HasCallStack => PropertyT IO () -> Property
unitProperty = withTests 1 . withDiscards 1 . property

-- | Make a test from a single property.
mkTest :: HasCallStack => Property -> Test
mkTest = Leaf

-- | Label a test. Can be nested. A "." is placed between nested
-- scopes, so @scope "foo" . scope "bar"@ is equivalent to @scope "foo.bar"@
scope :: String -> Test -> Test
scope msg tree =
  let newScopes = splitSpecifier msg
  in foldr (\scope' test -> Internal [(scope', test)]) tree newScopes

-- | Split a test specifier into parts
splitSpecifier :: String -> [String]
splitSpecifier str = case splitOn "." str of
  [""] -> []
  lst  -> lst

-- | Run a unit test. Example:
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
unitTest = mkTest . unitProperty

io :: IO a -> PropertyT IO a
io = liftIO

-- | Run a property test. Example:
--
-- >>> run $ propertyTest $ do
-- >>>   list <- forAll $ Gen.list @_ @Int (Range.linear 0 100)
-- >>>     (Gen.element [0..100])
-- >>>   reverse (reverse list) === list
-- > ━━━ run ━━━
-- >   ✓ (unnamed) passed 100 tests.
-- >   ✓ 1 succeeded.
propertyTest :: HasCallStack => PropertyT IO () -> Test
propertyTest = mkTest . property

-- | Record a success if 'True', otherwise record a failure
expect :: HasCallStack => Bool -> Test
expect False = withFrozenCallStack $ crash "unexpected"
expect True  = ok

-- | Record a success if 'Just', otherwise record a failure
expectJust :: HasCallStack => Maybe a -> Test
expectJust Nothing  = withFrozenCallStack $ crash "expected Just, got Nothing"
expectJust (Just _) = ok

-- | Record a success if 'Right', otherwise record a failure
expectRight :: (Show e, HasCallStack) => Either e a -> Test
expectRight (Left e)  = withFrozenCallStack $
  crash $ "expected Right, got (Left " ++ show e ++ ")"
expectRight (Right _) = ok

-- | Record a success if 'Right', otherwise record a failure
expectRightNoShow :: (HasCallStack) => Either e a -> Test
expectRightNoShow (Left _)  = withFrozenCallStack $
  crash $ "expected Right, got Left"
expectRightNoShow (Right _) = ok

-- | Record a success if 'Left', otherwise record a failure
expectLeft :: (Show a, HasCallStack) => Either e a -> Test
expectLeft (Right a) = withFrozenCallStack $
  crash $ "expected Left, got (Right " ++ show a ++ ")"
expectLeft (Left _)  = ok

-- | Record a success if 'Left', otherwise record a failure
expectLeftNoShow :: HasCallStack => Either e a -> Test
expectLeftNoShow (Right _) = withFrozenCallStack $
  crash $ "expected Left, got Right"
expectLeftNoShow (Left _)  = ok

-- | Record a success if both arguments are equal, otherwise record a failure.
--
-- This is nicer than @'expect' $ _ '==' _@ for equality tests because it can
-- provide a diff:
--
-- > ━━━ run ━━━
-- > ✗ (unnamed) failed after 1 test.
-- >
-- >      ┏━━ tests/Suite.hs ━━━
-- >   26 ┃ main :: IO ()
-- >   27 ┃ main = run $ expectEq "foo\nbar\nbaz" ("foo\nquux\nbaz" :: String)
-- >      ┃ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- >      ┃ │ Failed (- lhs =/= + rhs)
-- >      ┃ │ - "foo\nbar\nbaz"
-- >      ┃ │ + "foo\nquux\nbaz"
-- >
-- >   This failure can be reproduced by running:
-- >   > recheck (Size 0) (Seed 3595725845167890780 3451893767486943501) (unnamed)
-- >
-- > ✗ 1 failed.
expectEq :: (Eq a, Show a, HasCallStack) => a -> a -> Test
expectEq a b = withFrozenCallStack $ unitTest $ a === b

-- | Record a success if the arguments are not equal, otherwise record a
-- failure.
expectNeq :: (Eq a, Show a, HasCallStack) => a -> a -> Test
expectNeq a b = withFrozenCallStack $ unitTest $ a /== b

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
  Sequence trees -> concatMap (runTree' stack) trees
  Internal trees -> concatMap go trees
  Skipped test   -> skipTree' stack test
  where go (name, tree) = runTree' (name:stack) tree

-- | Flatten a subtree of tests. Use with 'mkGroup'
runTreeOnly :: [String] -> Test -> [([String], Property)]
runTreeOnly = runTreeOnly' [] where

  -- Note: In this first case, we override a skip if this test is specifically
  -- run
  runTreeOnly' stack []              tree             = runTree' stack tree
  runTreeOnly' stack (_:_)           tree@Leaf{}      = skipTree' stack tree
  runTreeOnly' stack scopes          (Sequence trees)
    = concatMap (runTreeOnly' stack scopes) trees
  runTreeOnly' stack _               (Skipped tree)   = skipTree' stack tree
  runTreeOnly' stack (scope':scopes) (Internal trees)
    = concatMap go trees
    where go (name, tree) =
            if name == scope'
            then runTreeOnly' (name:stack) scopes tree
            else skipTree'    (name:stack)        tree

-- | Skip this test tree (mark all properties as skipped).
skipTree' :: [String] -> Test -> [([String], Property)]
skipTree' stack = \case
  Leaf _prop     -> [(reverse stack, unitProperty discard)]
  Sequence trees -> concatMap (skipTree' stack) trees
  Internal trees -> concatMap go trees
  Skipped test   -> skipTree' stack test

  where go (name, tree) = skipTree' (name:stack) tree

-- | Run all tests whose scope starts with the given prefix
runOnly :: String -> Test -> IO ()
runOnly prefix t = do
  let props = runTreeOnly (splitSpecifier prefix) t
      group = mkGroup (fromString $ "runOnly " ++ show prefix) props

  seed <- random
  recheckSeed seed group

-- | Rerun all tests with the given seed and whose scope starts with the given
-- prefix
rerunOnly :: String -> Seed -> Test -> IO ()
rerunOnly prefix seed t = do
  let props = runTreeOnly (splitSpecifier prefix) t
      name = fromString $ "rerunOnly " ++ show prefix
  recheckSeed seed $ mkGroup name props

-- | Run all tests
run :: Test -> IO ()
run t = do
  seed <- random
  recheckSeed seed $ mkGroup "run" $ runTree t

-- | Rerun all tests with the given seed
rerun :: Seed -> Test -> IO ()
rerun -- seed t = recheckSeed seed $ mkGroup "run" $ runTree t
  = rerunOnly ""

-- -- | Log a string
-- note :: MonadTest m => String -> m ()
-- note = footnote

-- -- | Log a showable value
-- noteShow :: (MonadTest m, Show s) => s -> m ()
-- noteShow = footnoteShow

-- | Record a successful test
ok :: Test
ok = unitTest success

-- | Explicitly skip this test.
skip :: Test -> Test
skip = Skipped

-- | Mark a test as pending.
pending :: String -> Test
pending msg = unitTest $ do { footnote msg; discard }

-- | Record a failure with a given message
crash :: HasCallStack => String -> Test
crash msg = withFrozenCallStack $
  Leaf $ unitProperty $ do { footnote msg; failure }
