{-# language Rank2Types #-}
{-# language ScopedTypeVariables #-}
module EasyTest.Generators
  ( -- * Generators
    random
  , random'
  , bool
  , word8
  , char
  , int
  , double
  , word
  , int'
  , char'
  , double'
  , word'
  , word8'
  , pick
  , listOf
  , listsOf
  , pair
  , mapOf
  , mapsOf
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map (Map)
import Data.Maybe ( fromJust )
import Data.Word
import System.Random (Random)
import qualified Data.Map as Map
import qualified System.Random as Random

import EasyTest.Internal

-- | Generate a random value
random :: forall a. Random a => Test a
random = do
  rng <- asks envRng
  liftIO . atomically $ do
    rng0 <- readTVar rng
    let (a :: a, rng1) = Random.random rng0
    writeTVar rng rng1
    pure a

-- | Generate a bounded random value. Inclusive on both sides.
random' :: Random a => a -> a -> Test a
random' lower upper = do
  rng <- asks envRng
  liftIO . atomically $ do
    rng0 <- readTVar rng
    let (a, rng1) = Random.randomR (lower,upper) rng0
    writeTVar rng rng1
    pure a

bool :: Test Bool
bool = random

word8 :: Test Word8
word8 = random

-- | Generate a random 'Char'
char :: Test Char
char = random

-- | Generate a random 'Int'
int :: Test Int
int = random

-- | Generate a random 'Double'
double :: Test Double
double = random

-- | Generate a random 'Word'
word :: Test Word
word = random

-- | Generate a random 'Int' in the given range
-- Note: @int' 0 5@ includes both @0@ and @5@
int' :: Int -> Int -> Test Int
int' = random'

-- | Generate a random 'Char' in the given range
-- Note: @char' 'a' 'z'@ includes both @'a'@ and @'z'@.
char' :: Char -> Char -> Test Char
char' = random'

-- | Generate a random 'Double' in the given range
-- Note: @double' 0 1@ includes both @0@ and @1@.
double' :: Double -> Double -> Test Double
double' = random'

-- | Generate a random 'Double' in the given range
-- Note: @word' 0 10@ includes both @0@ and @10@.
word' :: Word -> Word -> Test Word
word' = random'

-- | Generate a random 'Double' in the given range
-- Note: @word8' 0 10@ includes both @0@ and @10@.
word8' :: Word8 -> Word8 -> Test Word8
word8' = random'

-- | Sample uniformly from the given list of possibilities
pick :: [a] -> Test a
pick as = let n = length as; ind = picker n as in do
  i <- int' 0 (n - 1)
  a <- pure (ind i)
  pure (fromJust a)             -- TODO: fromJust is not a total function

picker :: Int -> [a] -> (Int -> Maybe a)
picker _ [] = const Nothing
picker _ [a] = \i -> if i == 0 then Just a else Nothing
picker size as = go where
  lsize = size `div` 2
  rsize = size - lsize
  (l,r) = splitAt lsize as
  lpicker = picker lsize l
  rpicker = picker rsize r
  go i = if i < lsize then lpicker i else rpicker (i - lsize)

-- | Alias for 'replicateM'
listOf :: Int -> Test a -> Test [a]
listOf = replicateM

-- | Generate a list of lists of the given sizes,
-- an alias for @sizes \`forM\` \\n -> listOf n gen@
listsOf :: [Int] -> Test a -> Test [[a]]
listsOf sizes gen = sizes `forM` \n -> listOf n gen

-- | Alias for @liftA2 (,)@.
pair :: Test a -> Test b -> Test (a,b)
pair = liftA2 (,)

-- | Generate a @Data.Map k v@ of the given size.
mapOf :: Ord k => Int -> Test k -> Test v -> Test (Map k v)
mapOf n k v = Map.fromList <$> listOf n (pair k v)

-- | Generate a @[Data.Map k v]@ of the given sizes.
mapsOf :: Ord k => [Int] -> Test k -> Test v -> Test [Map k v]
mapsOf sizes k v = sizes `forM` \n -> mapOf n k v
