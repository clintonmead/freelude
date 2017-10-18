{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import Freelude.Impl.Category
import Freelude.Impl.ExoFunctor
import Prelude hiding ((.), fmap, pure, (=<<))

main :: IO ()
main = pure ()

f1 :: (IsSemigroupoid t1 p a2 a3,  IsSemigroupoid t2 p a1 a2, IsSemigroupoid t3 p a1 a3) => t1 -> t2 -> t3
f1 x y = x . y
f1' x y = x . y

f2 :: (IsSemigroupoid t1 p a3 a4, IsSemigroupoid t2 p a2 a3, IsSemigroupoid t3 p a1 a2, IsSemigroupoid t4 p a1 a4) => t1 -> t2 -> t3 -> t4
f2 x y z = x . y . z
f2' x y z = x . y . z

f3 x y = fmap x . fmap y
f3' x y = fmap x . fmap y

f4 x y = fmap (x . y)
f4' x y = fmap (x . y)

g :: a -> a -> a
g = undefined

f5 x y = g (f3 x y) (f4 x y)

f6 x y = exomap x . exomap y
f6' x y = exomap x . exomap y

f7 x y = exomap (x . y)
f7' x y = exomap (x . y)

f8 x y = g (f6 x y) (f7 x y)
