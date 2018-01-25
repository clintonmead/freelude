{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Freelude.Impl.Compat (
  ) where

import Prelude hiding (Functor, fmap, (<$>))

import qualified Freelude.Impl.Classes as Class
import Freelude.Impl.Classes (
  FunctionP,
  BasicFunctorP,
  FunctorT,
  fmap,
  (<$>),
  FunctorSrcC, FunctorDstC
  )

class    Class.Functor FunctionP (BasicFunctorP t) => Functor t
instance Class.Functor FunctionP (BasicFunctorP t) => Functor t

class    (Class.Functor FunctionP p, FunctorSrcC p a, FunctorDstC p a, t ~ Class.FunctorT p a) => MonoFunctor' t p a
instance (Class.Functor FunctionP p, FunctorSrcC p a, FunctorDstC p a, t ~ Class.FunctorT p a) => MonoFunctor' t p a
