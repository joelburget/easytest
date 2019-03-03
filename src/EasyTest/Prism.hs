{-# LANGUAGE Rank2Types        #-}
-- |
-- Module      : EasyTest.Prism
-- Copyright   : (c) Joel Burget, 2018-2019; Edward Kmett 2012-2016
-- License     : MIT
-- Maintainer  : joelburget@gmail.com
-- Stability   : experimental
--
-- This module defines lens-style prisms for use with 'EasyTest.matches' /
-- 'EasyTest.doesn'tMatch'. These are equivalent and compatible with the
-- definitions in <http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Prism.html Control.Lens.Prism>.

-- Copyright 2012-2016 Edward Kmett
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
-- IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
-- ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
module EasyTest.Prism
  ( _Left
  , _Right
  , _Just
  , _Nothing
  , only
  , nearly
  , Prism
  , Prism'
  ) where

import           Control.Monad          (guard)
import           Data.Profunctor        (dimap, right')
import           EasyTest.Internal      (Prism, Prism')

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))
{-# INLINE prism' #-}

-- | 'Prism' to the 'Left' half of an 'Either'
_Left :: Prism (Either a c) (Either b c) a b
_Left = prism Left $ either Right (Left . Right)
{-# INLINE _Left #-}

-- | 'Prism' to the 'Right' half of an 'Either'
_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right $ either (Left . Left) Right
{-# INLINE _Right #-}

-- | 'Prism' to the 'Just' in a 'Maybe'
_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (Left Nothing) Right
{-# INLINE _Just #-}

-- | 'Prism' to the 'Nothing' in a 'Maybe'
_Nothing :: Prism' (Maybe a) ()
_Nothing = prism' (const Nothing) $ maybe (Just ()) (const Nothing)
{-# INLINE _Nothing #-}

-- | This 'Prism' compares for exact equality with a given value.
--
-- >>> only 4 # ()
-- 4
--
-- >>> 5 ^? only 4
-- Nothing
only :: Eq a => a -> Prism' a ()
only a = prism' (\() -> a) $ guard . (a ==)
{-# INLINE only #-}

-- | This 'Prism' compares for approximate equality with a given value and a predicate for testing,
-- an example where the value is the empty list and the predicate checks that a list is empty (same
-- as 'Control.Lens.Empty._Empty' with the 'Control.Lens.Empty.AsEmpty' list instance):
--
-- >>> nearly [] null # ()
-- []
-- >>> [1,2,3,4] ^? nearly [] null
-- Nothing
--
-- @'nearly' [] 'Prelude.null' :: 'Prism'' [a] ()@
--
-- To comply with the 'Prism' laws the arguments you supply to @nearly a p@ are somewhat constrained.
--
-- We assume @p x@ holds iff @x ≡ a@. Under that assumption then this is a valid 'Prism'.
--
-- This is useful when working with a type where you can test equality for only a subset of its
-- values, and the prism selects such a value.
nearly :: a -> (a -> Bool) -> Prism' a ()
nearly a p = prism' (\() -> a) $ guard . p
{-# INLINE nearly #-}
