{-|
Module      : EasyTest
Copyright   : (c) Joel Burget, 2018-2019
License     : MIT
Maintainer  : joelburget@gmail.com
Stability   : provisional

EasyTest is a simple testing toolkit for unit- and property-testing. It's based on the <http://hackage.haskell.org/package/hedgehog hedgehog> property-testing system. Here's an example usage:

@
module Main where

import           EasyTest
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

suite :: 'Test'
suite = 'tests'
  [ 'scope' "addition.ex" $ 'unitTest' $ 1 + 1 === 2
  , 'scope' "list.reversal" $ 'property' $ do
      ns @<-@ 'forAll' $
        Gen.list (Range.singleton 10) (Gen.int Range.constantBounded)
      reverse (reverse ns) '===' ns
  -- equivalent to `'scope' "addition.ex3"`
  , 'scope' "addition" . 'scope' "ex3" $ 'unitTest' $ 3 + 3 === 6
  , 'scope' "always passes" $ 'unitTest' 'success' -- record a success result
  , 'scope' "failing test" $ 'unitTest' $ 'crash' "oh noes!!"
  ]

-- NB: `'run' suite` would run all tests, but we only run
-- tests whose scopes are prefixed by "addition"
main :: IO 'Summary'
main = 'runOnly' "addition" suite
@

This generates the output:

> ━━━ runOnly "addition" ━━━
>   ✓ addition.ex1 passed 1 test.
>   ✓ addition.ex2 passed 1 test.
>   ⚐ list.reversal gave up after 1 discard, passed 0 tests.
>   ✓ addition.ex3 passed 1 test.
>   ⚐ always passes gave up after 1 discard, passed 0 tests.
>   ⚐ failing test gave up after 1 discard, passed 0 tests.
>   ⚐ 3 gave up, 3 succeeded.

We write tests with ordinary Haskell code, with control flow explicit and under programmer control.

= User guide

EasyTest supports two types of tests -- property tests and unit tests. Both are expressed as hedgehog properties ('PropertyT' 'IO' @()@). Unit tests, built with 'unitTest' (or 'example') are run once. Property tests, built with 'property', are run with many random values.

We often want to label tests so we can see when they succeed or fail. For that we use 'scope':

@
-- | Label a test. Can be nested. A `'.'` is placed between nested
-- scopes, so `scope "foo" . scope "bar"` is equivalent to `scope "foo.bar"`
'scope' :: String -> 'Test' -> 'Test'
@

Here's an example usage:

@
module Main where

import EasyTest
  ('Test', 'scope', 'crash', 'run', 'tests', 'example', 'success', ('==='), 'Summary')

suite :: 'Test'
suite = 'tests'
  [ 'example' 'success'
  , 'scope' "test-crash" $ 'example' $ 'crash' "oh noes!"
  , 'example' $ 1 + 1 === 2
  ]

main :: IO 'Summary'
main = 'run' suite
@

This example runs the three examples in order so that they're all tested. The output is:

> ━━━ run ━━━
>   ✓ (unnamed) passed 1 test.
>   ✗ test-crash failed after 1 test.
>
>        ┏━━ tests/Suite.hs ━━━
>      6 ┃ suite :: Test
>      7 ┃ suite = tests
>      8 ┃   [ example success
>      9 ┃   , scope "test-crash" $ example $ crash "oh noes!"
>        ┃   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
>     10 ┃   , example $ 1 + 1 === 2
>     11 ┃   ]
>
>     oh noes!
>
>     This failure can be reproduced by running:
>     > recheck (Size 0) (Seed 12444749623322829837 10053881125821732685) test-crash
>
>   ✓ (unnamed) passed 1 test.
>   ✗ 1 failed, 2 succeeded.

In the output, we get a stack trace pointing to the line where crash was called (@..tests/Suite.hs:9@), information about failing tests, and instructions for rerunning the tests with an identical random seed (in this case, there's no randomness, so @rerun@ would work fine, but if our test generated random data, we might want to rerun with the exact same random numbers). Note that, somewhat embarrassingly, the error message currently gives bad instructions and the correct way to rerun the tests is with @'rerun' (Seed 12444749623322829837 10053881125821732685) suite@.

The various run functions ('run', 'runOnly', 'rerun', and 'rerunOnly') all return a hedgehog 'Summary'. Use 'cabalTestSuite' to exit the process with a nonzero status in the event of a failure, for use with @exitcode-stdio-1.0@ cabal @test-suite@s. Here's an example cabal file:

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

For tests that are logically separate, we usually combine them into a suite using 'tests', as in:

@
suite = 'tests'
  [ 'scope' "ex1" $ 'example' $ 1 + 1 === 2
  , 'scope' "ex2" $ 'example' $ 2 + 2 === 4
  ]
@

== Property tests

We can also create property tests (via hedgehog). As an example, we can express the property that reversing a list twice results in the original list:

@
reverseTest :: Test ()
reverseTest = 'scope' "list reversal" $ 'property' $ do
  nums <- 'forAll' $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 99))
  reverse (reverse nums) '===' nums
@

The above code generates lists of sizes between 0 and 100, consisting of @Int@ values in the range 0 through 99.

If our list reversal test failed, we might use @'runOnly' "list reversal"@ or @'rerunOnly' "list reversal" \<randomseed\>@ to rerun just that subtree of the test suite, and we might add some additional diagnostics to see what was going on:

@
import           EasyTest
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

reverseTest :: Test ()
reverseTest = 'property' $ do
  nums <- 'forAll' $
    Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 99))
  'footnote' $ "nums: " ++ show nums
  let r = reverse (reverse nums)
  'footnote' $ "reverse (reverse nums): " ++ show r
  r '===' nums
@

See the <http://hackage.haskell.org/package/hedgehog hedgehog docs> for more on writing good property tests.

== Bracketed tests

EasyTest also supports /bracketed/ tests requiring setup and teardown.

For example, we could open a temporary file:

@
'scope' "bracket-example" $ 'example' $ 'bracket'
  (mkstemp "temp")
  (\(filepath, handle) -> hClose handle >> removeFile filepath)
  (\(_filepath, handle) -> do
    liftIO $ hPutStrLn handle "this temporary file will be cleaned up"
    'success')
@

'bracket' ensures that the resource is cleaned up, even if the test throws an
exception. You can write either property- or unit- tests in this style.

-}

module EasyTest (
  -- * Structuring tests
    Test
  , tests
  , scope
  , example
  , unitTest
  , property
  -- * Assertions for unit tests
  , matches
  , doesn'tMatch
  , skip
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
  -- * Hedgehog re-exports
  -- | These common functions are included as a convenience for writing
  -- 'propertyTest's. See "Hedgehog" for more.
  , success
  , failure
  , assert
  , (===)
  , (/==)
  , Seed(..)
  , Summary
  , footnote
  , annotate
  , forAll
  , PropertyT
  ) where

import           EasyTest.Internal
import           Hedgehog          hiding (Test, property)
