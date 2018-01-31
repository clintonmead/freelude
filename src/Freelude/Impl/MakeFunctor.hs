{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Freelude.Impl.MakeFunctor (
--  MakeFunctor(getFunctor)
) where
  {-
import Prelude hiding (Functor, fmap, Monad)
import qualified Prelude
import Data.Kind (Type)
import Freelude.Impl.Classes
import qualified Control.Applicative

newtype MakeFunctor f a = MakeFunctor { getFunctor :: f a }

data MakeFunctorP (f :: Type -> Type)

type instance FunctorT (MakeFunctorP f) a = MakeFunctor f a
type instance FunctorSrcC' (MakeFunctorP _) = 'Nothing
type instance FunctorDstC' (MakeFunctorP _) = 'Nothing

instance Prelude.Functor f => Functor FunctionP (MakeFunctorP f) where
  fmap f (MakeFunctor x) = MakeFunctor (Prelude.fmap f x)

instance Prelude.Applicative f => Lift (MakeFunctorP f) where
  liftA2 f (MakeFunctor x) (MakeFunctor y) = MakeFunctor (Control.Applicative.liftA2 f x y)

instance Prelude.Applicative f => Apply (MakeFunctorP f) where
  (MakeFunctor f) <*> (MakeFunctor x) = MakeFunctor (f Prelude.<*> x)

instance Prelude.Applicative f => Pure (MakeFunctorP f) where
  pure x = MakeFunctor (Prelude.pure x)

instance Prelude.Monad f => Monad (MakeFunctorP f) where
  (MakeFunctor x) >>= f = MakeFunctor (x Prelude.>>= f') where
    f' x' = getFunctor (f x')
-}
