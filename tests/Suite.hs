{-# language OverloadedStrings #-}
module Main where

import EasyTest
import Control.Monad

suite1 :: Test ()
suite1 = tests
  [ scope "a" ok
  , scope "b.c" ok
  , scope "b" ok
  , scope "b" . scope "c" . scope "d" $ ok
  , scope "c" ok
  ]

reverseTest :: Test ()
reverseTest = scope "list reversal" $ do
  lists <- listsOf [0..100] (int' 0 99)
  forM_ lists $ \nums -> expect (reverse (reverse nums) == nums)

main :: IO ()
main = do
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
