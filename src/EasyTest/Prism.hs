{-# LANGUAGE Rank2Types        #-}
module EasyTest.Prism
  ( _Left
  , _Right
  , _Just
  , _Nothing
  , Prism
  , Prism'
  ) where

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
