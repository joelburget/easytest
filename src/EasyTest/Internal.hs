{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
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
  , Testable(..)
  -- * Running tests
  , run
  , runOnly
  , rerun
  , rerunOnly
  -- * Assertions for unit tests
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
  -- * Bracketed tests (requiring setup / teardown)
  , bracket
  , bracket_
  , finally
  -- * Internal
  , TestType(..)
  , Test(..)
  , BracketedTest(..)
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

import qualified Control.Exception      as Ex
import           Control.Monad.Except
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Writer
import           Control.Monad.Trans.Maybe
import           Data.Functor           ((<&>))
import           Data.List              (intercalate)
import           Data.List.Split        (splitOn)
import           Data.String            (fromString)
#if MIN_VERSION_base(4,9,0)
import           GHC.Stack
#else
import           Data.CallStack
#endif

import           Hedgehog               hiding (Test, test)
import           Hedgehog.Internal.Gen hiding (discard)
import           Hedgehog.Internal.Property hiding (Test, propertyTest, test)
import           Hedgehog.Internal.Seed (random)
import qualified Hedgehog.Internal.Tree as HT

import           EasyTest.Hedgehog

-- | Properties that can be lifted into unit or property tests.
class Testable t where
  -- | This generalizes 'unitTest' to also handle 'bracket'ed tests. The type
  -- specializes to:
  --
  -- > mkUnitTest :: PropertyT IO () -> Test
  -- > mkUnitTest :: BracketedTest -> Test
  mkUnitTest     :: t -> Test
  -- | This generalizes 'propertyTest' to also handle 'bracket'ed tests. The
  -- type specializes to:
  --
  -- > mkPropertyTest :: PropertyT IO () -> Test
  -- > mkPropertyTest :: BracketedTest -> Test
  mkPropertyTest :: t -> Test

instance Testable BracketedTest where
  mkUnitTest     = Bracketed Unit . id
  mkPropertyTest = Bracketed Prop . id

instance Testable (PropertyT IO ()) where
  mkUnitTest     = Bracketed Unit . mkSimplyBracketedTest
  mkPropertyTest = Bracketed Prop . mkSimplyBracketedTest

-- | Unit- or property- test.
data TestType = Unit | Prop

-- | A set of unit- and property-tests
data Test
  = NamedTests ![(String, Test)]
  -- ^ A set of named (scoped) tests
  | Sequence ![Test]
  -- ^ A sequence of tests
  | Bracketed !TestType !BracketedTest
  -- ^ A test with setup and teardown
  | Skipped !Test
  -- ^ A set of tests marked to skip

-- | A test with setup and teardown steps.
data BracketedTest where
  BracketedTest
    :: IO a
    -> (a -> IO ())
    -> (a -> PropertyT IO ())
    -> BracketedTest

-- | Make a test with setup and teardown steps.
bracket :: IO a -> (a -> IO ()) -> (a -> PropertyT IO ()) -> BracketedTest
bracket setup teardown test = BracketedTest setup teardown test

-- | A variant of 'bracket' where the return value from the setup step is not
-- required.
bracket_ :: IO a -> IO b -> PropertyT IO () -> BracketedTest
bracket_ before after thing
  = bracket before (\_ -> do { _ <- after; pure () }) (const thing)

-- | A specialised variant of 'bracket' with just a teardown step.
finally :: PropertyT IO () -> IO a -> BracketedTest
finally test after = bracket_ (pure ()) after test

-- | A degenerate bracketed test with no meaningful setup or teardown.
mkSimplyBracketedTest :: HasCallStack => PropertyT IO () -> BracketedTest
mkSimplyBracketedTest p = BracketedTest
  (pure ())
  (\() -> pure ())
  (\() -> p)

-- | Label a test. Can be nested. A "." is placed between nested
-- scopes, so @scope "foo" . scope "bar"@ is equivalent to @scope "foo.bar"@
scope :: String -> Test -> Test
scope msg tree =
  let newScopes = splitSpecifier msg
  in foldr (\scope' test -> NamedTests [(scope', test)]) tree newScopes

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
unitTest = mkUnitTest

-- | Conventient alias for 'liftIO'.
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
propertyTest = mkPropertyTest

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
-- This is nicer than @'expect' $ _ == _@ for equality tests because it can
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
mkGroup :: GroupName -> [([String], TestType, BracketedTest)] -> Group
mkGroup name props = Group name $ props <&> \(path, ty, bracketed) ->
  let name' = case path of
        [] -> "(unnamed)"
        _  -> fromString (intercalate "." path)
      propConf = case ty of
        Unit -> PropertyConfig 1 1 0 0
        Prop -> defaultConfig
  in (name', Property propConf (mkProperty bracketed))

-- | Tear down a 'PropertyT', installing the cleanup handler, then build it
-- back up.
mkProperty :: BracketedTest -> PropertyT IO ()
mkProperty (BracketedTest setup teardown test)
  = PropertyT $ TestT $ ExceptT $ WriterT $ GenT $ \size seed ->
      HT.Tree $ MaybeT $ do
        a <- setup
        case test a of
          PropertyT (TestT (ExceptT (WriterT (GenT innerTest)))) -> Ex.finally
            (runMaybeT $ HT.runTree $ innerTest size seed)
            (teardown a)

-- | Flatten a test tree. Use with 'mkGroup'
runTree :: Test -> [([String], TestType, BracketedTest)]
runTree = runTree' []

runTree' :: [String] -> Test -> [([String], TestType, BracketedTest)]
runTree' stack = \case
  Bracketed ty prop -> [(reverse stack, ty, prop)]
  Sequence trees -> concatMap (runTree' stack) trees
  NamedTests trees -> concatMap go trees
  Skipped test   -> skipTree' stack test
  where go (name, tree) = runTree' (name:stack) tree

-- | Flatten a subtree of tests. Use with 'mkGroup'
runTreeOnly :: [String] -> Test -> [([String], TestType, BracketedTest)]
runTreeOnly = runTreeOnly' [] where

  -- Note: In this first case, we override a skip if this test is specifically
  -- run
  runTreeOnly' stack []              tree             = runTree' stack tree
  runTreeOnly' stack (_:_)           tree@Bracketed{} = skipTree' stack tree
  runTreeOnly' stack scopes          (Sequence trees)
    = concatMap (runTreeOnly' stack scopes) trees
  runTreeOnly' stack _               (Skipped tree)   = skipTree' stack tree
  runTreeOnly' stack (scope':scopes) (NamedTests trees)
    = concatMap go trees
    where go (name, tree) =
            if name == scope'
            then runTreeOnly' (name:stack) scopes tree
            else skipTree'    (name:stack)        tree

-- | Skip this test tree (mark all properties as skipped).
skipTree' :: [String] -> Test -> [([String], TestType, BracketedTest)]
skipTree' stack = \case
  Bracketed ty _test -> [(reverse stack, ty, mkSimplyBracketedTest discard)]
  Sequence trees  -> concatMap (skipTree' stack) trees
  NamedTests trees  -> concatMap go trees
  Skipped test    -> skipTree' stack test

  where go (name, tree) = skipTree' (name:stack) tree

-- | Run all tests whose scope starts with the given prefix.
--
-- >>> runOnly "components.a" tests
runOnly :: String -> Test -> IO ()
runOnly prefix t = do
  let props = runTreeOnly (splitSpecifier prefix) t
      group = mkGroup (fromString $ "runOnly " ++ show prefix) props

  seed <- random
  recheckSeed seed group

-- | Rerun all tests with the given seed and whose scope starts with the given
-- prefix
--
-- >>> rerunOnly "components.a" (Seed 2914818620245020776 12314041441884757111) tests
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
--
-- >>> rerun (Seed 2914818620245020776 12314041441884757111) tests
rerun :: Seed -> Test -> IO ()
rerun = rerunOnly ""

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
crash msg = withFrozenCallStack $ mkUnitTest
  (do { footnote msg; failure } :: PropertyT IO ())
