**Note: this library is no longer maintained**

[![Hackage][hackage-shield]][hackage] [![Travis][travis-shield]][travis]

EasyTest is a simple testing toolkit for unit- and property-testing. It's based on the [hedghog](http://hackage.haskell.org/package/hedgehog) property-testing system. Here's an example usage:

```haskell
module Main where

import           EasyTest
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

suite :: Test
suite = tests
  [ scope "addition.ex" $ unitTest $ 1 + 1 === 2
  , scope "list.reversal" $ property $ do
      ns <- forAll $
        Gen.list (Range.singleton 10) (Gen.int Range.constantBounded)
      reverse (reverse ns) === ns
  -- equivalent to `scope "addition.ex3"`
  , scope "addition" . scope "ex3" $ unitTest $ 3 + 3 === 6
  , scope "always passes" $ unitTest success -- record a success result
  , scope "failing test" $ unitTest $ crash "oh noes!!"
  ]

-- NB: `run suite` would run all tests, but we only run
-- tests whose scopes are prefixed by "addition"
main :: IO Summary
main = runOnly "addition" suite
```

This generates the output:

```
━━━ runOnly "addition" ━━━
  ✓ addition.ex1 passed 1 test.
  ✓ addition.ex2 passed 1 test.
  ⚐ list.reversal gave up after 1 discard, passed 0 tests.
  ✓ addition.ex3 passed 1 test.
  ⚐ always passes gave up after 1 discard, passed 0 tests.
  ⚐ failing test gave up after 1 discard, passed 0 tests.
  ⚐ 3 gave up, 3 succeeded.
```

We write tests with ordinary Haskell code, with control flow explicit and under programmer control.

## User guide

EasyTest supports two types of tests -- property tests and unit tests. Both are expressed as hedgehog properties (`PropertyT IO ()`). Unit tests, built with `unitTest` (or `example`) are run once. Property tests, built with `property`, are run with many random values.

We often want to label tests so we can see when they succeed or fail. For that we use `scope`:

```haskell
-- | Label a test. Can be nested. A `.` is placed between nested
-- scopes, so `scope "foo" . scope "bar"` is equivalent to `scope "foo.bar"`
scope :: String -> Test -> Test
```

Here's an example usage:

```haskell
module Main where

import EasyTest
  (Test, scope, crash, run, tests, example, success, (===), Summary)

suite :: Test
suite = tests
  [ example success
  , scope "test-crash" $ example $ crash "oh noes!"
  , example $ 1 + 1 === 2
  ]

main :: IO Summary
main = run suite
```

This example runs the three examples in order so that they're all tested. The output is:

```
━━━ run ━━━
  ✓ (unnamed) passed 1 test.
  ✗ test-crash failed after 1 test.

       ┏━━ tests/Suite.hs ━━━
     6 ┃ suite :: Test
     7 ┃ suite = tests
     8 ┃   [ example success
     9 ┃   , scope "test-crash" $ example $ crash "oh noes!"
       ┃   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    10 ┃   , example $ 1 + 1 === 2
    11 ┃   ]

    oh noes!

    This failure can be reproduced by running:
    > recheck (Size 0) (Seed 12444749623322829837 10053881125821732685) test-crash

  ✓ (unnamed) passed 1 test.
  ✗ 1 failed, 2 succeeded.
```

In the output, we get a stack trace pointing to the line where crash was called (`..tests/Suite.hs:9`), information about failing tests, and instructions for rerunning the tests with an identical random seed (in this case, there's no randomness, so `rerun` would work fine, but if our test generated random data, we might want to rerun with the exact same random numbers). Note that, somewhat embarrassingly, the error message currently gives bad instructions and the correct way to rerun the tests is with `rerun (Seed 12444749623322829837 10053881125821732685) suite`.

The various run functions (`run`, `runOnly`, `rerun`, and `rerunOnly`) all return a hedgehog `Summary`. Use `cabalTestSuite` to exit the process with a nonzero status in the event of a failure, for use with `exitcode-stdio-1.0` cabal `test-suite`s. Here's an example cabal file:

```
test-suite tests
  type:           exitcode-stdio-1.0
  main-is:        NameOfYourTestSuite.hs
  hs-source-dirs: tests
  other-modules:
  build-depends:
    base,
    easytest
```

For tests that are logically separate, we usually combine them into a suite using `tests`, as in:

```haskell
suite = tests
  [ scope "ex1" $ example $ 1 + 1 === 2
  , scope "ex2" $ example $ 2 + 2 === 4
  ]
```

### Property tests

We can also create property tests (via hedgehog). As an example, we can express the property that reversing a list twice results in the original list:

```haskell
reverseTest :: Test ()
reverseTest = scope "list reversal" $ property $ do
  nums <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 99))
  reverse (reverse nums) === nums
```

The above code generates lists of sizes between 0 and 100, consisting of `Int` values in the range 0 through 99.

If our list reversal test failed, we might use `runOnly "list reversal"` or `rerunOnly "list reversal" <randomseed>` to rerun just that subtree of the test suite, and we might add some additional diagnostics to see what was going on:

```haskell
import           EasyTest
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

reverseTest :: Test ()
reverseTest = property $ do
  nums <- forAll $
    Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 99))
  footnote $ "nums: " ++ show nums
  let r = reverse (reverse nums)
  footnote $ "reverse (reverse nums): " ++ show r
  r === nums
```

See the [hedgehog docs](http://hackage.haskell.org/package/hedgehog) for more on writing good property tests.

### Bracketed tests

EasyTest also supports _bracketed_ tests requiring setup and teardown.

For example, we could open a temporary file:

```haskell
scope "bracket-example" $ example $ bracket
  (mkstemp "temp")
  (\(filepath, handle) -> hClose handle >> removeFile filepath)
  (\(_filepath, handle) -> do
    liftIO $ hPutStrLn handle "this temporary file will be cleaned up"
    success)
```

`bracket` ensures that the resource is cleaned up, even if the test throws an
exception. You can write either property- or unit- tests in this style.

### <a id="rationale">Why?

Here's some of my thinking in the design of this library:

* Testing should uncomplicated, minimal friction, and ideally: FUN. If I have to think too much or remember arbitrary framework magic, I get irritated.
* A lot of testing frameworks are weirdly optimized for adding lots of diagnostic information up front, as if whatever diagnostic information you happen to think to capture will be exactly what is needed to fix whatever bugs your tests reveal. In my experience this is almost never the case, so EasyTest takes the opposite approach: be EXTREMELY LAZY about adding diagnostics and labeling subexpressions, but make it trivial to reproduce failing tests without running your entire suite. If a test fails, you can easily rerun just that test, with the exact same random seed, and add whatever diagnostics or print statements you need to track down what's wrong. And EasyTest helpfully tells you how to do this rerunning whenever your tests fail, because otherwise I'd never remember. (Again: keep the friction LOW!)
* Another reason not to add diagnostics up front: you avoid needing to remember two different versions of every function or operator (the one you use in your regular code, and the one you use with your testing "framework" to supply diagnostics). HUnit has operators named `(@=?)`, `(~?=)`, and a bunch of others for asserting equality with diagnostics on failure. QuickCheck has `(.&&.)` and `(.||.)`. Just... no.
* HUnit, QuickCheck, SmallCheck, Tasty, and whatever else are frameworks that hide control flow from the programmer and make some forms of control flow difficult or impossible to specify (for instance, you can't do I/O in your regular QuickCheck tests... unless you use `Test.QuickCheck.Monadic`, which has yet another API you have to learn!). In contrast, EasyTest is just a single data type with a monadic API and a few helper functions. You assemble your tests using ordinary monadic code, and there is never any magic. Want to abstract over something? _Write a regular function._ Need to generate some testing data? Write regular functions.
* "How do I modify the number of generated test cases for QuickCheck for just one of my properties?" Or control the maximum size for these `Gen` and `Arbitrary` types? Some arbitrary "configuration setting" that you have to look up every time. No thanks!
* Seriously, global configuration settings are evil! I want fine-grained control over the amount of parallelism, test case sizes, and so on. And if I find I'm repeating myself a lot... I'll _introduce a regular Haskell variable or function!_. DOWN WITH FRAMEWORKS AND THEIR DAMN CONFIGURATION SETTINGS!!
* Most of the functionality of QuickCheck is overkill anyway! There's no need for `Arbitrary` instances (explicit generation is totally fine, and even preferred in most cases), `Coarbitrary` (cute, but not useful when the HOF you are testing is parametric), or shrinking (just generate your test cases in increasing sizes, and your first failure will be the smallest!).

I hope that you enjoy writing your tests with this library!

[hackage]: https://hackage.haskell.org/package/easytest
[hackage-shield]: https://img.shields.io/badge/hackage-v0.2.1.svg?style=flat

[travis]: https://travis-ci.org/joelburget/easytest
[travis-shield]: https://travis-ci.org/joelburget/easytest.svg?branch=master&style=flat
