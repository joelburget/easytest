{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
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
  , skip
  , example
  , unitTest
  , property
  , propertyWith
  -- * Assertions for unit tests
  , matches
  , doesn'tMatch
  , pending
  , crash
  -- * Running tests
  , run
  , runOnly
  , rerun
  , rerunOnly
  -- * Bracketed tests (requiring setup / teardown)
  , bracket
  , bracket_
  , finally
  -- * Cabal test suite
  , cabalTestSuite
  -- * Internal
  , Prism
  , Prism'
  , TestType(..)
  , Test(..)
  -- * Hedgehog re-exports
  , Property
  , PropertyT
  , MonadTest
  , (===)
  , (/==)
  , Seed
  , Summary(..)
  , PropertyConfig(..)
  , defaultConfig
  ) where

import           Control.Applicative        (Const(..))
import qualified Control.Exception          as Ex
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Data.List                  (intercalate)
import           Data.List.Split            (splitOn)
import           Data.Monoid (First(..))
import           Data.Profunctor.Choice
import           Data.Profunctor.Unsafe
import           Data.String                (fromString)
#if MIN_VERSION_base(4,9,0)
import           GHC.Stack
#else
import           Data.CallStack
#endif
import           System.Exit

import           Hedgehog                   hiding (Test, test, property)
import           Hedgehog.Internal.Gen      hiding (discard)
import           Hedgehog.Internal.Property hiding (Test, property, test)
import           Hedgehog.Internal.Report   (Summary (..))
import           Hedgehog.Internal.Seed     (random)
import qualified Hedgehog.Internal.Tree     as HT

import           EasyTest.Internal.Hedgehog

-- | A prism embodies one constructor of a sum type (as a lens embodies one
-- part of a product type). See 'EasyTest.Prism._Just', 'EasyTest.Prism._Nothing', 'EasyTest.Prism._Left', and 'EasyTest.Prism._Right' for examples. See <http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Prism.html Control.Lens.Prism> for more explanation.
type Prism     s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

-- | A type-restricted prism. See <http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Prism.html Control.Lens.Prism> for more explanation.
type Prism'    s   a   = Prism s s a a

type Getting r s   a   = (a -> Const r a) -> s -> Const r s

-- | Unit- or property- test.
data TestType = Unit | Prop PropertyConfig

-- | A set of unit- and property-tests
data Test
  = NamedTests ![(String, Test)]
  -- ^ A set of named (scoped) tests
  | Sequence ![Test]
  -- ^ A sequence of tests
  | Leaf !TestType !(PropertyT IO ())
  -- ^ An atomic unit- or property-test
  | Skipped !Test
  -- ^ A set of tests marked to skip

-- | Run a list of tests
tests :: [Test] -> Test
tests = Sequence

-- | Label a test. Can be nested. A "." is placed between nested
-- scopes, so @scope "foo" . scope "bar"@ is equivalent to @scope "foo.bar"@
scope :: String -> Test -> Test
scope msg tree =
  let newScopes = splitSpecifier msg
  in foldr (\scope' test -> NamedTests [(scope', test)]) tree newScopes

-- | Run a unit test (same as 'unitTest'). Example:
--
-- >>> run $ example $ 1 === 2
-- > ━━━ run ━━━
-- >   ✗ (unnamed) failed after 1 test.
-- >
-- >        ┏━━ tests/Suite.hs ━━━
-- >     26 ┃ main :: IO ()
-- >     27 ┃ main = do
-- >     28 ┃   run $ example $ 1 === (2 :: Int)
-- >        ┃   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- >        ┃   │ Failed (- lhs =/= + rhs)
-- >        ┃   │ - 1
-- >        ┃   │ + 2
-- >
-- >     This failure can be reproduced by running:
-- >     > recheck (Size 0) (Seed 2914818620245020776 12314041441884757111) (unnamed)
-- >
-- >   ✗ 1 failed.
example :: HasCallStack => PropertyT IO () -> Test
example = Leaf Unit

-- | Run a unit test (same as 'example'). Example:
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
unitTest = Leaf Unit

-- | Run a property test. Example:
--
-- >>> run $ scope "list reversal" $ property $ do
-- >..   list <- forAll $ Gen.list @_ @Int (Range.linear 0 100)
-- >..     (Gen.element [0..100])
-- >..   reverse (reverse list) === list
-- > ━━━ run ━━━
-- >   ✓ list reversal passed 100 tests.
-- >   ✓ 1 succeeded.
property :: HasCallStack => PropertyT IO () -> Test
property = Leaf (Prop defaultConfig)

-- | Run a property test with a custom configuration. This allows you to configure the 'propertyTestLimit', 'propertyDiscardLimit', 'propertyShrinkLimit', or 'propertyShrinkRetries'. Example:
--
-- >>> run $ scope "list reversal" $ propertyWith (defaultConfig { propertyTestLimit = 500 }) $ do
-- >..   list <- forAll $ Gen.list @_ @Int (Range.linear 0 100)
-- >..     (Gen.element [0..100])
-- >..   reverse (reverse list) === list
-- > ━━━ run ━━━
-- >   ✓ list reversal passed 500 tests.
-- >   ✓ 1 succeeded.
propertyWith :: HasCallStack => PropertyConfig -> PropertyT IO () -> Test
propertyWith = Leaf . Prop

-- | Make a test with setup and teardown steps.
bracket :: IO a -> (a -> IO ()) -> (a -> PropertyT IO ()) -> PropertyT IO ()
bracket setup teardown test
  = PropertyT $ TestT $ ExceptT $ WriterT $ GenT $ \size seed ->
      HT.Tree $ MaybeT $ do
        a <- setup
        case test a of
          PropertyT (TestT (ExceptT (WriterT (GenT innerTest)))) -> Ex.finally
            (runMaybeT $ HT.runTree $ innerTest size seed)
            (teardown a)

-- | A variant of 'bracket' where the return value from the setup step is not
-- required.
bracket_ :: IO a -> IO b -> PropertyT IO () -> PropertyT IO ()
bracket_ before after thing
  = bracket before (\_ -> do { _ <- after; pure () }) (const thing)

-- | A specialised variant of 'bracket' with just a teardown step.
finally :: PropertyT IO () -> IO a -> PropertyT IO ()
finally test after = bracket_ (pure ()) after test

-- | Split a test specifier into parts
splitSpecifier :: String -> [String]
splitSpecifier str = case splitOn "." str of
  [""] -> []
  lst  -> lst

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf l f = getConst #. l (Const #. f)
{-# INLINE foldMapOf #-}

preview :: Getting (First a) s a -> s -> Maybe a
preview l = getFirst #. foldMapOf l (First #. Just)
{-# INLINE preview #-}

-- | Test whether a 'Prism' matches. Example:
--
-- >>> main
-- > ━━━ run ━━━
-- >   ✓ (unnamed) passed 1 test.
-- >   ✗ (unnamed) failed after 1 test.
-- >
-- >        ┏━━ tests/Suite.hs ━━━
-- >     48 ┃ main :: IO ()
-- >     49 ┃ main = do
-- >     50 ┃   _ <- run $ tests
-- >     51 ┃     [ expect $ matches _Left (Left 1   :: Either Int ())
-- >     52 ┃     , expect $ matches _Left (Right () :: Either Int ())
-- >        ┃     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- >     53 ┃     ]
-- >     54 ┃   pure ()
-- >
-- >     Prism failed to match
-- >
-- >     This failure can be reproduced by running:
-- >     > recheck (Size 0) (Seed 14003809197113786240 2614482618840800713) (unnamed)
-- >
-- >   ✗ 1 failed, 1 succeeded.
--
-- Use with 'EasyTest.Prism._Just', 'EasyTest.Prism._Nothing', 'EasyTest.Prism._Left', 'EasyTest.Prism._Right', or <http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Prism.html Control.Lens.Prism>
matches :: HasCallStack => Prism' s a -> s -> PropertyT IO ()
matches p s = withFrozenCallStack $ case preview p s of
  Just _  -> success
  Nothing -> do { footnote "Prism failed to match"; failure }

-- | Test whether a 'Prism' doesn't match. Compare with 'matches'.
doesn'tMatch :: HasCallStack => Prism' s a -> s -> PropertyT IO ()
doesn'tMatch p s = withFrozenCallStack $ case preview p s of
  Nothing -> success
  Just _  -> do { footnote "Prism matched"; failure }

-- | Make a 'Hedgehog.Group' from a list of tests.
mkGroup :: GroupName -> [([String], TestType, PropertyT IO ())] -> Group
mkGroup name props = Group name $ props <&> \(path, ty, test) ->
  let name' = case path of
        [] -> "(unnamed)"
        _  -> fromString (intercalate "." path)
      propConf = case ty of
        Unit      -> PropertyConfig 1 1 0 0
        Prop conf -> conf
  in (name', Property propConf test)

-- | Flatten a test tree. Use with 'mkGroup'
runTree :: Test -> [([String], TestType, PropertyT IO ())]
runTree = runTree' []

runTree' :: [String] -> Test -> [([String], TestType, PropertyT IO ())]
runTree' stack = \case
  Leaf ty prop     -> [(reverse stack, ty, prop)]
  Sequence trees   -> concatMap (runTree' stack) trees
  NamedTests trees -> concatMap go trees
  Skipped test     -> skipTree' stack test
  where go (name, tree) = runTree' (name:stack) tree

-- | Flatten a subtree of tests. Use with 'mkGroup'
runTreeOnly :: [String] -> Test -> [([String], TestType, PropertyT IO ())]
runTreeOnly = runTreeOnly' [] where

  -- Note: In this first case, we override a skip if this test is specifically
  -- run
  runTreeOnly' stack []              tree           = runTree' stack tree
  runTreeOnly' stack (_:_)           tree@Leaf{}    = skipTree' stack tree
  runTreeOnly' stack scopes          (Sequence trees)
    = concatMap (runTreeOnly' stack scopes) trees
  runTreeOnly' stack _               (Skipped tree) = skipTree' stack tree
  runTreeOnly' stack (scope':scopes) (NamedTests trees)
    = concatMap go trees
    where go (name, tree) =
            if name == scope'
            then runTreeOnly' (name:stack) scopes tree
            else skipTree'    (name:stack)        tree

-- | Skip this test tree (mark all properties as skipped).
skipTree' :: [String] -> Test -> [([String], TestType, PropertyT IO ())]
skipTree' stack = \case
  -- As a minor hack, we set any skipped test to type 'Unit' so it'll only run
  -- once.
  Leaf _ty _test   -> [(reverse stack, Unit, discard)]
  Sequence trees   -> concatMap (skipTree' stack) trees
  NamedTests trees -> concatMap go trees
  Skipped test     -> skipTree' stack test

  where go (name, tree) = skipTree' (name:stack) tree

-- | Run all tests whose scope starts with the given prefix.
--
-- >>> runOnly "components.a" tests
runOnly :: String -> Test -> IO Summary
runOnly prefix t = do
  let props = runTreeOnly (splitSpecifier prefix) t
      group = mkGroup (fromString $ "runOnly " ++ show prefix) props

  seed <- random
  recheckSeed seed group

-- | Rerun all tests with the given seed and whose scope starts with the given
-- prefix
--
-- >>> rerunOnly "components.a" (Seed 2914818620245020776 12314041441884757111) tests
rerunOnly :: String -> Seed -> Test -> IO Summary
rerunOnly prefix seed t = do
  let props = runTreeOnly (splitSpecifier prefix) t
      name = fromString $ "rerunOnly " ++ show prefix
  recheckSeed seed $ mkGroup name props

-- | Run all tests
run :: Test -> IO Summary
run t = do
  seed <- random
  recheckSeed seed $ mkGroup "run" $ runTree t

-- | Rerun all tests with the given seed
--
-- >>> rerun (Seed 2914818620245020776 12314041441884757111) tests
rerun :: Seed -> Test -> IO Summary
rerun = rerunOnly ""

-- | Explicitly skip this set of tests.
skip :: Test -> Test
skip = Skipped

-- | Mark a test as pending.
pending :: String -> PropertyT IO ()
pending msg = do { footnote msg; discard }

-- | Record a failure with a given message
crash :: HasCallStack => String -> PropertyT IO ()
crash msg = withFrozenCallStack $ do { footnote msg; failure }

-- | Make this a cabal test suite for use with @exitcode-stdio-1.0@
-- @test-suite@s.
--
-- This simply checks to see if any tests failed and if so exits with
-- 'exitFailure'.
cabalTestSuite :: IO Summary -> IO ()
cabalTestSuite getSummary = do
  summary <- getSummary
  if summaryFailed summary > 0 then exitFailure else pure ()

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
