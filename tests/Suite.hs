{-# language OverloadedStrings   #-}
{-# language TypeApplications    #-}
{-# language ScopedTypeVariables #-}
module Main where

import           EasyTest
import           Hedgehog          (forAll, (===))
import qualified Hedgehog.Gen      as Gen
import qualified Hedgehog.Range    as Range

suite1 :: Test
suite1 = tests
  [ scope "a" ok
  , scope "b.c" ok
  , scope "b" ok
  , scope "b" . scope "c" . scope "d" $ ok
  ]

reverseTest :: Test
reverseTest = scope "list reversal" $ propertyTest $ do
  list <- forAll $ Gen.list @_ @Int (Range.linear 0 100)
    (Gen.element [0..100])
  reverse (reverse list) === list

main :: IO ()
main = do
  run $ expect $ 1 == (1 :: Int)
  run suite1
  runOnly "a" suite1
  runOnly "b" suite1
  runOnly "b" $ tests [suite1, scope "xyz" (crash "never run")]
  runOnly "b.c" $ tests [suite1, scope "b" (crash "never run")]
  runOnly "x.go" $ tests
    [ scope "x.go to" (crash "never run")
    , scope "x.go" ok
    ]
  runOnly "x.go to" $ tests
    [ scope "x.go to" ok
    , scope "x.go" (crash "never run")
    ]
  run reverseTest

  run $ tests
    [ expectLeft        (Left 1   :: Either Int ())
    , expectLeftNoShow  (Left 2   :: Either Int ())
    , expectRight       (Right () :: Either Int ())
    , expectRightNoShow (Right () :: Either Int ())

    -- Uncomment for an example diff:
    -- , expectEq          "foo\nbar\nbaz" ("foo\nquux\nbaz" :: String)
    ]
  run $ scope "typecheck-me" $ skip $ unitTest $ do
    io $ putStrLn "we can do IO (though printing is a bad idea)"
    success
  pure ()
