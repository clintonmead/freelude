{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleContexts #-}

module Freelude.Impl.ExoFunctor where

import Freelude.Impl.Category
import Prelude hiding (id, ($), (<*>), (<$>))
import Data.Functor.Identity (Identity(Identity))

class ExoFunctor c1 c2 where
  exomap :: (ExoCategoryC c1 a b, ExoCategoryC c2 a b) => ExoCategoryT c1 a b -> ExoCategoryT c2 a b

infixr 0 $
($) :: (ExoFunctor c1 c2, ExoCategoryC c1 a b, ExoCategoryC c2 a b) => ExoCategoryT c1 a b -> ExoCategoryT c2 a b
($) = exomap

instance ExoFunctor c c where
  exomap = id

instance ExoFunctor c1 c2 => ExoFunctor (Identity c1) c2 where
  exomap (Identity x) = exomap x

instance ExoFunctor c1 c2 => ExoFunctor c1 (Identity c2) where
  exomap x = Identity (exomap x)

instance ExoFunctor c1 c2 => ExoFunctor (Identity c1) (Identity c2) where
  exomap (Identity x) = Identity (exomap x)

instance (ExoFunctor p1 FunctionP, ExoFunctor p2 FunctionP) => ExoFunctor (p1, p2) FunctionP where
  exomap (f1, f2) (x1, x2) = (f1 $ x1, f2 $ x2)

instance (ExoFunctor p1 FunctionP, ExoFunctor p2 FunctionP, ExoFunctor p3 FunctionP) => ExoFunctor (p1, p2, p3) FunctionP where
  exomap (f1, f2, f3) (x1, x2, x3) = (f1 $ x1, f2 $ x2, f3 $ x3)

instance (ExoFunctor p1 FunctionP, ExoFunctor p2 FunctionP, ExoFunctor p3 FunctionP, ExoFunctor p4 FunctionP) => ExoFunctor (p1, p2, p3, p4) FunctionP where
  exomap (f1, f2, f3, f4) (x1, x2, x3, x4) = (f1 $ x1, f2 $ x2, f3 $ x3, f4 $ x4)

instance (ExoFunctor p1 FunctionP, ExoFunctor p2 FunctionP, ExoFunctor p3 FunctionP, ExoFunctor p4 FunctionP, ExoFunctor p5 FunctionP) => ExoFunctor (p1, p2, p3, p4, p5) FunctionP where
  exomap (f1, f2, f3, f4, f5) (x1, x2, x3, x4, x5) = (f1 $ x1, f2 $ x2, f3 $ x3, f4 $ x4, f5 $ x5)

instance (ExoFunctor p FunctionP) => ExoFunctor (FunctorCategoryP (BasicFunctor Maybe) p) FunctionP where
  exomap f x = (exomap <$> f) <*> x

instance (ExoFunctor p FunctionP) => ExoFunctor (FunctorCategoryP (BasicFunctor []) p) FunctionP where
  exomap f x = (exomap <$> f) <*> x
