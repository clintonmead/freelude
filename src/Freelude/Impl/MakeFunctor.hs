{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Freelude.Impl.MakeFunctor where

import Freelude.Impl.Functor
import Prelude hiding (Functor, fmap)
import qualified Prelude
import Data.Kind (Type)
import Freelude.Impl.Category

newtype MakeFunctor f a = MakeFunctor { getFunctor :: f a }

data MakeFunctorP (f :: Type -> Type)

type instance FunctorT (MakeFunctorP f) a = MakeFunctor f a
type instance FunctorSrcC (MakeFunctorP _) _ = ()
type instance FunctorDstC (MakeFunctorP _) _ = ()

instance Prelude.Functor f => Functor FunctionP (MakeFunctorP f) where
  fmap f (MakeFunctor x) = MakeFunctor (Prelude.fmap f x)
