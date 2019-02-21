{-|
Module      : EasyTest
Copyright   : (c) Joel Burget, 2018-2019
License     : MIT
Maintainer  : joelburget@gmail.com
Stability   : provisional

EasyTest is a simple testing toolkit, meant to replace most uses of QuickCheck, SmallCheck, HUnit, and frameworks like Tasty, etc. Here's an example usage:

TODO: test

@
module Main where

import EasyTest
import Control.Applicative
import Control.Monad
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

suite :: Test ()
suite = tests
  [ scope "addition.ex1" $ unitTest $ 1 + 1 === 2
  , scope "addition.ex2" $ unitTest $ 2 + 3 === 5
  , scope "list.reversal" $ propertyTest $ do
      -- generate lists from size 0 to 10, of Ints in (0,43)
      -- shorthand: listsOf [0..10] (int' 0 43)
      ns @<-@ Gen.list (Range.singleton 10) (Gen.int Range.constantBounded)
      reverse (reverse ns) === ns
  -- equivalent to `scope "addition.ex3"`
  , scope "addition" . scope "ex3" $ unitTest $ 3 + 3 === 6
  , scope "always passes" $
      note "I'm running this test, even though it always passes!" $
      ok -- record a success result
  , scope "failing test" $ crash "oh noes!!"
  ]

-- NB: `run suite` would run all tests, but we only run
-- tests whose scopes are prefixed by "addition"
main = runOnly "addition" suite
@

This generates the output:

TODO: update

@
Randomness seed for this run is 5104092164859451056
Raw test output to follow ...
------------------------------------------------------------
OK addition.ex1
OK addition.ex2
OK addition.ex3
------------------------------------------------------------
✅  3 tests passed, no failures! 👍 🎉
@

The idea here is to write tests with ordinary Haskell code, with control flow explicit and under programmer control. Tests are values of type @Test a@, and @Test@ forms a monad with access to:

= User guide

The simplest tests are @ok@, @crash@, and @expect@:

@
-- Record a success
ok :: Test

-- Record a failure
crash :: String -> Test

-- Record a success if True, otherwise record a failure
expect :: Bool -> Test
@

We often want to label tests so we can see when they succeed or fail. For that we use @scope@:

@
-- | Label a test. Can be nested. A `'.'` is placed between nested
-- scopes, so `scope "foo" . scope "bar"` is equivalent to `scope "foo.bar"`
scope :: String -> Test -> Test
@

Here's an example usage, putting all these primitives together:

@
module Main where

import EasyTest (ok, scope, crash, expect, run)

suite :: Test
suite = tests
  [ ok
  , scope "test-crash" $ crash "oh noes!"
  , expect $ 1 + 1 == 2
  ]

main = run suite
@

This example is sequencing the @ok@, @crash@, and @expect@, so that they're all tested. The output is:

TODO: update

@
Randomness seed for this run is 1830293182471192517
Raw test output to follow ...
------------------------------------------------------------
test-crash FAILURE oh noes! CallStack (from HasCallStack):
  crash, called at @/@Users@/@pchiusano@/@code@/@easytest@/@tests@/@Suite.hs:10:24 in main:Main
OK
FAILED test-crash
------------------------------------------------------------


  1 passed
  1 FAILED (failed scopes below)
    "test-crash"

  To rerun with same random seed:

    EasyTest.rerun 1830293182471192517
    EasyTest.rerunOnly 1830293182471192517 "test-crash"


------------------------------------------------------------
❌
@

In the output (which is streamed to the console), we get a stack trace pointing to the line where crash was called (@..tests/Suite.hs:10:24@), information about failing tests, and instructions for rerunning the tests with an identical random seed (in this case, there's no randomness, so @rerun@ would work fine, but if our test generated random data, we might want to rerun with the exact same random numbers).

TODO: do this

The various run functions (@run@, @runOnly@, @rerun@, and @rerunOnly@) all exit the process with a nonzero status in the event of a failure, so they can be used for continuous integration or test running tools that key off the process exit code to determine whether the suite succeeded or failed. For instance, here's the relevant portion of a typical cabal file:

@
test-suite tests
  type:           exitcode-stdio-1.0
  main-is:        NameOfYourTestSuite.hs
  hs-source-dirs: tests
  other-modules:
  build-depends:
    base,
    easytest
@

For tests that are logically separate, we usually combine them into a suite using @tests@ (which is just @msum@), as in:

@
suite = tests
  [ scope "ex1" $ expect (1 + 1 == 2)
  , scope "ex2" $ expect (2 + 2 == 4)
  ]
@

We often want to generate random data for testing purposes:

@
reverseTest :: Test ()
reverseTest = scope "list reversal" $ do
  nums <- Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 99))
  reverse (reverse nums) === nums
@

The above code generates lists of sizes between 0 and 100, consisting of @Int@ values in the range 0 through 99.

If our list reversal test failed, we might use @'runOnly' "list reversal"@ or @'rerunOnly' "list reversal" \<randomseed\>@ to rerun just that subtree of the test suite, and we might add some additional diagnostics to see what was going on:

@
reverseTest :: Test ()
reverseTest = scope "list reversal" $ do
  nums <- Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 99))
  footnote $ "nums: " ++ show nums
  let r = reverse (reverse nums)
  footnote $ "reverse (reverse nums): " ++ show r
  r === nums
@

-}

module EasyTest (
  -- * Tests
    Test
  -- * Structuring tests
  , tests
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
  -- * Hedgehog re-exports
  , (===)
  , (/==)
  , Seed
  , forAll
  , forAllWith
  ) where

import           EasyTest.Internal
import Hedgehog hiding (Test)
