{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Freelude.Impl.Category {-(
  Semigroupoid((.)),
  Category(id),
  Arr(arr),
  FunctionP,
  CategoryT, ExoCategoryT,
  CategorySrcC, ExoCategorySrcC,
  CategoryDstC, ExoCategoryDstC,
  CategoryC, ExoCategoryC,
  IsSemigroupoid, IsCategory
)-} where

import qualified Control.Category as Base (Category((.), id))

import Data.Type.Equality ((:~:))
import Data.Type.Coercion (Coercion)
import Control.Arrow (Kleisli)
import Data.Monoid (Dual(Dual))
import qualified Control.Arrow

import Control.IndexT.Tuple (TupleConstraint)
import Control.IndexT (IndexT)
import Control.IndexT.Constructor (IndexC)

import Data.Functor.Identity (Identity(Identity))
import Prelude hiding (Functor(fmap), (<$>), Applicative((<*>), pure), Monad, (=<<), (.), id)
import qualified Prelude
import GHC.Exts (Constraint)
import Data.Kind (Type)
import Data.Set (Set)
import qualified Data.Set
import Data.Functor.Constant (Constant)
import Freelude.Impl.ToKind (ToType)
import Data.List.NonEmpty (NonEmpty)
import Data.Tree (Tree)
import Data.Semigroup (Option, Min, Max, Last, First)
import GHC.TypeLits (Nat)

type family CategoryT (p :: Type) (a :: Type) (b :: Type) = (f :: Type) | f -> p a b
type family ExoCategoryT (p :: Type) (a :: Type) (b :: Type) = (f :: Type) | f -> p a b
type family CategorySrcC (p :: Type) (a :: Type) :: Constraint
type family CategoryDstC (p :: Type) (b :: Type) :: Constraint
type family ExoCategorySrcC (p :: Type) (a :: Type) :: Constraint
type family ExoCategoryDstC (p :: Type) (b :: Type) :: Constraint
type CategoryC p a b = (CategorySrcC p a, CategoryDstC p b)
type ExoCategoryC p a b = (ExoCategorySrcC p a, ExoCategoryDstC p b)
type IsSemigroupoid t p a b = (Semigroupoid p, t ~ CategoryT p a b, CategoryC p a b)
type ExoIsSemigroupoid t p a b = (Semigroupoid p, t ~ ExoCategoryT p a b, ExoCategoryC p a b)
type IsCategory t p a b = (IsSemigroupoid t p a b, Category p)
type ExoIsCategory t p a b = (ExoIsSemigroupoid t p a b, Category p)

class Semigroupoid p where
  (.) :: (IsSemigroupoid t1 p b c, IsSemigroupoid t2 p a b, IsSemigroupoid t3 p a c) => t1 -> t2 -> t3

class Semigroupoid p => Category p where
  id :: (IsCategory t p a a, ExoIsCategory t p a a) => t

class Category p => Arr p where
  arr :: CategoryC p a b => (a -> b) -> CategoryT p a b

-- * Instances from 'Control.Category'
-- ** '->'

data FunctionP

type instance CategoryT FunctionP a b = (->) a b
type instance ExoCategoryT FunctionP a b = (->) a b
type instance CategorySrcC FunctionP a = ()
type instance CategoryDstC FunctionP b = ()
type instance ExoCategorySrcC FunctionP a = ()
type instance ExoCategoryDstC FunctionP b = ()

instance Semigroupoid FunctionP where
  (.) = (Base..)
instance Category FunctionP where
  id = Base.id
instance Arr FunctionP where
  arr = id

-- ** 'Data.Equality.:-:'
data ProxyK k (a :: k)
data TypeEqP (k :: Type)

type instance CategoryT (TypeEqP k) (ProxyK k a) (ProxyK k b) = (:~:) a b
type instance ExoCategoryT (TypeEqP k) (ProxyK k a) (ProxyK k b) = (:~:) a b
type instance CategorySrcC (TypeEqP k) a = a ~ ProxyK k (IndexC 2 1 a)
type instance CategoryDstC (TypeEqP k) a = a ~ ProxyK k (IndexC 2 1 a)
type instance ExoCategorySrcC (TypeEqP k) a = CategorySrcC (TypeEqP k) a
type instance ExoCategoryDstC (TypeEqP k) a = CategoryDstC (TypeEqP k) a

instance Semigroupoid (TypeEqP k) where
  (.) = (Base..)
instance Category (TypeEqP k) where
  id = Base.id

data CoercionP (k :: Type)

type instance CategoryT (CoercionP k) (ProxyK k a) (ProxyK k b) = Coercion a b
type instance CategorySrcC (CoercionP k) a = a ~ ProxyK k (IndexC 2 1 a)
type instance CategoryDstC (CoercionP k) a = a ~ ProxyK k (IndexC 2 1 a)
type instance ExoCategoryT (CoercionP k) (ProxyK k a) (ProxyK k b) = Coercion a b
type instance ExoCategorySrcC (CoercionP k) a = CategorySrcC (TypeEqP k) a
type instance ExoCategoryDstC (CoercionP k) a = CategoryDstC (TypeEqP k) a

instance Semigroupoid (CoercionP k) where
  (.) = (Base..)
instance Category (CoercionP k) where
  id = Base.id

-- ** 'Control.Category.Kleisli'

data KleisliP (m :: Type -> Type)

type instance CategoryT (KleisliP m) a b = Kleisli m a b
type instance CategorySrcC (KleisliP _)  _ = ()
type instance CategoryDstC (KleisliP _)  _ = ()
type instance ExoCategoryT (KleisliP m) (m a) (m b) = Kleisli m a b
type instance ExoCategorySrcC (KleisliP _) _ = ()
type instance ExoCategoryDstC (KleisliP _) _ = ()

instance Prelude.Monad m => Semigroupoid (KleisliP m)  where
  (.) = (Base..)
instance Prelude.Monad m => Category (KleisliP m) where
  id = Base.id
instance Prelude.Monad m => Arr (KleisliP m) where
  arr = Control.Arrow.arr

-- * Data.Semigroup

-- ** 'Maybe'

data FunctorCategoryP (functorP :: Type) (p :: Type)

type instance CategorySrcC (FunctorCategoryP _ p) a = CategorySrcC p a
type instance CategoryDstC (FunctorCategoryP _ p) b = CategoryDstC p b
type instance ExoCategorySrcC (FunctorCategoryP functorP p) a = (ExoCategorySrcC p (IndexC 1 0 a), a ~ FunctorT functorP (IndexC 1 0 a))
type instance ExoCategoryDstC (FunctorCategoryP functorP p) b = (ExoCategoryDstC p (IndexC 1 0 b), b ~ FunctorT functorP (IndexC 1 0 b))

type instance CategoryT (FunctorCategoryP (BasicFunctor Maybe) p) a b = Maybe (CategoryT p a b)
type instance ExoCategoryT (FunctorCategoryP (BasicFunctor Maybe) p) (Maybe a) (Maybe b) = Maybe (ExoCategoryT p a b)

instance Semigroupoid p => Semigroupoid (FunctorCategoryP (BasicFunctor Maybe) p) where
  x . y = (.) <$> x <*> y
instance Semigroupoid p => Category (FunctorCategoryP (BasicFunctor Maybe) p) where
  id = Nothing
instance Arr (FunctorCategoryP (BasicFunctor Maybe) FunctionP) where
  arr = pure

-- ** Lists

type instance CategoryT (FunctorCategoryP (BasicFunctor []) p) a b = [CategoryT p a b]
type instance ExoCategoryT (FunctorCategoryP (BasicFunctor []) p) [a] [b] = [ExoCategoryT p a b]

instance Semigroupoid p => Semigroupoid (FunctorCategoryP (BasicFunctor []) p) where
  x . y = (.) <$> x <*> y
instance Semigroupoid p => Category (FunctorCategoryP (BasicFunctor []) p) where
  id = mempty
instance Arr (FunctorCategoryP (BasicFunctor []) FunctionP) where
  arr = pure

-- ** Identity

type instance CategoryT (Identity p) a b = Identity (CategoryT p a b)
type instance CategorySrcC (Identity p) a = CategorySrcC p a
type instance CategoryDstC (Identity p) b = CategoryDstC p b
type instance ExoCategoryT (Identity p) a b = Identity (ExoCategoryT p a b)
type instance ExoCategorySrcC (Identity p) a = ExoCategorySrcC p a
type instance ExoCategoryDstC (Identity p) b = ExoCategoryDstC p b

instance Semigroupoid p => Semigroupoid (Identity p) where
  x . y = (.) <$> x <*> y

instance Category p => Category (Identity p) where
  id = Identity id

instance Arr (Identity FunctionP) where
  arr = pure

-- ** Dual

type instance CategoryT (Dual p) a b = Dual (CategoryT p b a)
type instance CategorySrcC (Dual p) a = CategoryDstC p a
type instance CategoryDstC (Dual p) b = CategorySrcC p b
type instance ExoCategoryT (Dual p) a b = Dual (ExoCategoryT p b a)
type instance ExoCategorySrcC (Dual p) a = ExoCategoryDstC p a
type instance ExoCategoryDstC (Dual p) b = ExoCategorySrcC p b

instance Semigroupoid p => Semigroupoid (Dual p) where
  Dual x . Dual y = Dual (y . x)

instance Category p => Category (Dual p) where
  id = Dual id

-- ** Tuples

type instance CategoryT (p1, p2) (a1, a2) (b1, b2) = (CategoryT p1 a1 b1, CategoryT p2 a2 b2)
type instance CategorySrcC (p1, p2) a = (TupleConstraint 2 a, CategorySrcC p1 (IndexT 0 a), CategorySrcC p2 (IndexT 1 a))
type instance CategoryDstC (p1, p2) b = (TupleConstraint 2 b, CategoryDstC p1 (IndexT 0 b), CategoryDstC p2 (IndexT 1 b))
type instance ExoCategoryT (p1, p2) (a1, a2) (b1, b2) = (ExoCategoryT p1 a1 b1, ExoCategoryT p2 a2 b2)
type instance ExoCategorySrcC (p1, p2) a = (TupleConstraint 2 a, ExoCategorySrcC p1 (IndexT 0 a), ExoCategorySrcC p2 (IndexT 1 a))
type instance ExoCategoryDstC (p1, p2) b = (TupleConstraint 2 b, ExoCategoryDstC p1 (IndexT 0 b), ExoCategoryDstC p2 (IndexT 1 b))

instance (Semigroupoid p1, Semigroupoid p2) => Semigroupoid (p1, p2) where
  (x1, x2) . (y1, y2) = (x1 . y1, x2 . y2)

type instance CategoryT (p1, p2, p3) (a1, a2, a3) (b1, b2, b3) = (CategoryT p1 a1 b1, CategoryT p2 a2 b2, CategoryT p3 a3 b3)
type instance CategorySrcC (p1, p2, p3) a = (TupleConstraint 3 a, CategorySrcC p1 (IndexT 0 a), CategorySrcC p2 (IndexT 1 a), CategorySrcC p3 (IndexT 2 a))
type instance CategoryDstC (p1, p2, p3) b = (TupleConstraint 3 b, CategoryDstC p1 (IndexT 0 b), CategoryDstC p2 (IndexT 1 b), CategoryDstC p3 (IndexT 2 b))
type instance ExoCategoryT (p1, p2, p3) (a1, a2, a3) (b1, b2, b3) = (ExoCategoryT p1 a1 b1, ExoCategoryT p2 a2 b2, ExoCategoryT p3 a3 b3)
type instance ExoCategorySrcC (p1, p2, p3) a = (TupleConstraint 3 a, ExoCategorySrcC p1 (IndexT 0 a), ExoCategorySrcC p2 (IndexT 1 a), ExoCategorySrcC p3 (IndexT 2 a))
type instance ExoCategoryDstC (p1, p2, p3) b = (TupleConstraint 3 b, ExoCategoryDstC p1 (IndexT 0 b), ExoCategoryDstC p2 (IndexT 1 b), ExoCategoryDstC p3 (IndexT 2 b))

instance (Semigroupoid p1, Semigroupoid p2, Semigroupoid p3) => Semigroupoid (p1, p2, p3) where
  (x1, x2, x3) . (y1, y2, y3) = (x1 . y1, x2 . y2, x3 . y3)

type instance CategoryT (p1, p2, p3, p4) (a1, a2, a3, a4) (b1, b2, b3, b4) = (CategoryT p1 a1 b1, CategoryT p2 a2 b2, CategoryT p3 a3 b3, CategoryT p4 a4 b4)
type instance CategorySrcC (p1, p2, p3, p4) a = (TupleConstraint 4 a, CategorySrcC p1 (IndexT 0 a), CategorySrcC p2 (IndexT 1 a), CategorySrcC p3 (IndexT 2 a), CategorySrcC p4 (IndexT 3 a))
type instance CategoryDstC (p1, p2, p3, p4) b = (TupleConstraint 4 b, CategoryDstC p1 (IndexT 0 b), CategoryDstC p2 (IndexT 1 b), CategoryDstC p3 (IndexT 2 b), CategoryDstC p4 (IndexT 3 b))
type instance ExoCategoryT (p1, p2, p3, p4) (a1, a2, a3, a4) (b1, b2, b3, b4) = (ExoCategoryT p1 a1 b1, ExoCategoryT p2 a2 b2, ExoCategoryT p3 a3 b3, ExoCategoryT p4 a4 b4)
type instance ExoCategorySrcC (p1, p2, p3, p4) a = (TupleConstraint 4 a, ExoCategorySrcC p1 (IndexT 0 a), ExoCategorySrcC p2 (IndexT 1 a), ExoCategorySrcC p3 (IndexT 2 a), ExoCategorySrcC p4 (IndexT 3 a))
type instance ExoCategoryDstC (p1, p2, p3, p4) b = (TupleConstraint 4 b, ExoCategoryDstC p1 (IndexT 0 b), ExoCategoryDstC p2 (IndexT 1 b), ExoCategoryDstC p3 (IndexT 2 b), ExoCategoryDstC p4 (IndexT 3 b))

instance (Semigroupoid p1, Semigroupoid p2, Semigroupoid p3, Semigroupoid p4) => Semigroupoid (p1, p2, p3, p4) where
  (x1, x2, x3, x4) . (y1, y2, y3, y4) = (x1 . y1, x2 . y2, x3 . y3, x4 . y4)

type instance CategoryT (p1, p2, p3, p4, p5) (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5) = (CategoryT p1 a1 b1, CategoryT p2 a2 b2, CategoryT p3 a3 b3, CategoryT p4 a4 b4, CategoryT p5 a5 b5)
type instance CategorySrcC (p1, p2, p3, p4, p5) a = (TupleConstraint 5 a, CategorySrcC p1 (IndexT 0 a), CategorySrcC p2 (IndexT 1 a), CategorySrcC p3 (IndexT 2 a), CategorySrcC p4 (IndexT 3 a), CategorySrcC p5 (IndexT 4 a))
type instance CategoryDstC (p1, p2, p3, p4, p5) b = (TupleConstraint 5 b, CategoryDstC p1 (IndexT 0 b), CategoryDstC p2 (IndexT 1 b), CategoryDstC p3 (IndexT 2 b), CategoryDstC p4 (IndexT 3 b), CategoryDstC p5 (IndexT 4 b))
type instance ExoCategoryT (p1, p2, p3, p4, p5) (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5) = (ExoCategoryT p1 a1 b1, ExoCategoryT p2 a2 b2, ExoCategoryT p3 a3 b3, ExoCategoryT p4 a4 b4, ExoCategoryT p5 a5 b5)
type instance ExoCategorySrcC (p1, p2, p3, p4, p5) a = (TupleConstraint 5 a, ExoCategorySrcC p1 (IndexT 0 a), ExoCategorySrcC p2 (IndexT 1 a), ExoCategorySrcC p3 (IndexT 2 a), ExoCategorySrcC p4 (IndexT 3 a), ExoCategorySrcC p5 (IndexT 4 a))
type instance ExoCategoryDstC (p1, p2, p3, p4, p5) b = (TupleConstraint 5 b, ExoCategoryDstC p1 (IndexT 0 b), ExoCategoryDstC p2 (IndexT 1 b), ExoCategoryDstC p3 (IndexT 2 b), ExoCategoryDstC p4 (IndexT 3 b), ExoCategoryDstC p5 (IndexT 4 b))

instance (Semigroupoid p1, Semigroupoid p2, Semigroupoid p3, Semigroupoid p4, Semigroupoid p5) => Semigroupoid (p1, p2, p3, p4, p5) where
  (x1, x2, x3, x4, x5) . (y1, y2, y3, y4, y5) = (x1 . y1, x2 . y2, x3 . y3, x4 . y4, x5 . y5)

-- Functor

type family FunctorT (p :: Type) (a :: Type) = (b :: Type) | b -> p a

type family FunctorSrcC (p :: Type) (a :: Type) :: Constraint
type family FunctorDstC (p :: Type) (b :: Type) :: Constraint

--type FunctorC cat p a b ra rb = (ra ~ FunctorT p a, rb ~ FunctorT p b, CategoryC cat a b, CategoryC cat ra rb, FunctorSrcC p a, FunctorDstC p b)

class Semigroupoid cat => Functor cat p where
  fmap ::
    (ra ~ FunctorT p a, rb ~ FunctorT p b, CategoryC cat a b, CategoryC cat ra rb, FunctorSrcC p a, FunctorDstC p b) =>
    CategoryT cat a b -> CategoryT cat ra rb
  default fmap ::
    (Applicative cat p, ra ~ FunctorT p a, rb ~ FunctorT p b, CategoryC cat a b, CategoryC cat ra rb, FunctorSrcC p a, FunctorDstC p b) =>
    CategoryT cat a b -> CategoryT cat ra rb
  fmap f = (<*>) (pure f)

infixl 4 <$>
(<$>) ::
  (Functor cat p, ra ~ FunctorT p a, rb ~ FunctorT p b, CategoryC cat a b, CategoryC cat ra rb, FunctorSrcC p a, FunctorDstC p b) =>
  CategoryT cat a b -> CategoryT cat ra rb
(<$>) = fmap

infixl 4 <*>
class Functor cat p => Apply cat p where
  (<*>) ::
    (ra ~ FunctorT p a, rb ~ FunctorT p b, CategoryC cat a b, CategoryC cat ra rb, FunctorSrcC p a, FunctorDstC p b) =>
    FunctorT p (CategoryT cat a b) -> CategoryT cat ra rb

{-
  default (<*>) ::
    (ra ~ FunctorT p a, rb ~ FunctorT p b, CategoryC cat a b, CategoryC cat ra rb, FunctorSrcC p a, FunctorDstC p b) =>
    FunctorT p (CategoryT cat a b) -> CategoryT cat ra rb
  (<*>) = liftA2 id

  liftA2
    :: (Functor FunctionP p, Apply cat p,
        CategorySrcC cat a1, CategoryDstC cat b,
        CategorySrcC cat (FunctorT p a1), CategoryDstC cat (FunctorT p b),
        FunctorSrcC p a1, FunctorDstC p b, FunctorSrcC p a2,
        FunctorDstC p (CategoryT cat a1 b)) =>
       (a2 -> CategoryT cat a1 b) -> FunctorT p a2 -> CategoryT cat (FunctorT p a1) (FunctorT p b)

  liftA2 f x = (<*>) (fmap f x)
  -}

{-
  liftA2 ::
    (ra ~ FunctorT p a, rb ~ FunctorT p b, rc ~ FunctorT p c, CategoryC cat b c, CategoryC cat rb rc, CategoryC cat a (CategoryT cat b c), CategoryC cat ra (CategoryT cat rb rc)) =>
    CategoryT cat a (CategoryT cat b c) -> CategoryT cat ra (CategoryT cat rb rc)
  liftA2 f x = (<*>) (fmap f x)
-}
class Pure p where
  pure ::
    (ra ~ FunctorT p a) => a -> ra

type Applicative cat p = (Apply cat p, Pure p)

class Applicative cat p => Monad cat p where
  (=<<) ::
    (ra ~ FunctorT p a, rb ~ FunctorT p b, CategoryC cat a b, CategoryC cat ra rb) =>
    CategoryT cat a rb -> CategoryT cat ra rb



data BasicFunctor f
type instance FunctorSrcC (BasicFunctor _) _ = ()
type instance FunctorDstC (BasicFunctor _) _ = ()

data ConstantP (a :: Type)
type instance FunctorT (BasicFunctor (ConstantP a)) b = Constant a b
instance Functor FunctionP (BasicFunctor (ConstantP a)) where
  fmap = Prelude.fmap

type instance FunctorT (BasicFunctor Maybe) a = Maybe a
instance Functor FunctionP (BasicFunctor Maybe) where
  fmap = Prelude.fmap
instance Pure (BasicFunctor Maybe) where
  pure = Prelude.pure
instance Apply FunctionP (BasicFunctor Maybe) where
  (<*>) = (Prelude.<*>)
instance Monad FunctionP (BasicFunctor Maybe) where
  (=<<) = (Prelude.=<<)

type instance FunctorT (BasicFunctor Identity) a = Identity a
instance Functor FunctionP (BasicFunctor Identity) where
  fmap = Prelude.fmap
instance Pure (BasicFunctor Identity) where
  pure = Prelude.pure
instance Apply FunctionP (BasicFunctor Identity) where
  (<*>) = (Prelude.<*>)
instance Monad FunctionP (BasicFunctor Identity) where
  (=<<) = (Prelude.=<<)

type instance FunctorT (BasicFunctor NonEmpty) a = NonEmpty a
instance Functor FunctionP (BasicFunctor NonEmpty) where
  fmap = Prelude.fmap
instance Pure (BasicFunctor NonEmpty) where
  pure = Prelude.pure
instance Apply FunctionP (BasicFunctor NonEmpty) where
  (<*>) = (Prelude.<*>)
instance Monad FunctionP (BasicFunctor NonEmpty) where
  (=<<) = (Prelude.=<<)

type instance FunctorT (BasicFunctor (Either a)) b = Either a b
instance Functor FunctionP (BasicFunctor (Either a)) where
  fmap = Prelude.fmap
instance Pure (BasicFunctor (Either a)) where
  pure = Prelude.pure
instance Apply FunctionP (BasicFunctor (Either a)) where
  (<*>) = (Prelude.<*>)
instance Monad FunctionP (BasicFunctor (Either a)) where
  (=<<) = (Prelude.=<<)

type instance FunctorT (BasicFunctor []) a = [a]
instance Functor FunctionP (BasicFunctor []) where
  fmap = Prelude.fmap
instance Pure (BasicFunctor []) where
  pure = Prelude.pure
instance Apply FunctionP (BasicFunctor []) where
  (<*>) = (Prelude.<*>)
instance Monad FunctionP (BasicFunctor []) where
  (=<<) = (Prelude.=<<)

type instance FunctorT (BasicFunctor IO) a = IO a
instance Functor FunctionP (BasicFunctor IO) where
  fmap = Prelude.fmap
instance Pure (BasicFunctor IO) where
  pure = Prelude.pure
instance Apply FunctionP (BasicFunctor IO) where
  (<*>) = (Prelude.<*>)
instance Monad FunctionP (BasicFunctor IO) where
  (=<<) = (Prelude.=<<)

type instance FunctorT (BasicFunctor Option) a = Option a
instance Functor FunctionP (BasicFunctor Option) where
  fmap = Prelude.fmap
instance Pure (BasicFunctor Option) where
  pure = Prelude.pure
instance Apply FunctionP (BasicFunctor Option) where
  (<*>) = (Prelude.<*>)
instance Monad FunctionP (BasicFunctor Option) where
  (=<<) = (Prelude.=<<)

type instance FunctorT (BasicFunctor Tree) a = Tree a
instance Functor FunctionP (BasicFunctor Tree) where
  fmap = Prelude.fmap
instance Pure (BasicFunctor Tree) where
  pure = Prelude.pure
instance Apply FunctionP (BasicFunctor Tree) where
  (<*>) = (Prelude.<*>)
instance Monad FunctionP (BasicFunctor Tree) where
  (=<<) = (Prelude.=<<)

type instance FunctorT (BasicFunctor Min) a = Min a
instance Functor FunctionP (BasicFunctor Min) where
  fmap = Prelude.fmap
instance Pure (BasicFunctor Min) where
  pure = Prelude.pure
instance Apply FunctionP (BasicFunctor Min) where
  (<*>) = (Prelude.<*>)
instance Monad FunctionP (BasicFunctor Min) where
  (=<<) = (Prelude.=<<)

type instance FunctorT (BasicFunctor Max) a = Max a
instance Functor FunctionP (BasicFunctor Max) where
  fmap = Prelude.fmap
instance Pure (BasicFunctor Max) where
  pure = Prelude.pure
instance Apply FunctionP (BasicFunctor Max) where
  (<*>) = (Prelude.<*>)
instance Monad FunctionP (BasicFunctor Max) where
  (=<<) = (Prelude.=<<)

type instance FunctorT (BasicFunctor Last) a = Last a
instance Functor FunctionP (BasicFunctor Last) where
  fmap = Prelude.fmap
instance Pure (BasicFunctor Last) where
  pure = Prelude.pure
instance Apply FunctionP (BasicFunctor Last) where
  (<*>) = (Prelude.<*>)
instance Monad FunctionP (BasicFunctor Last) where
  (=<<) = (Prelude.=<<)

type instance FunctorT (BasicFunctor First) a = First a
instance Functor FunctionP (BasicFunctor First) where
  fmap = Prelude.fmap
instance Pure (BasicFunctor First) where
  pure = Prelude.pure
instance Apply FunctionP (BasicFunctor First) where
  (<*>) = (Prelude.<*>)
instance Monad FunctionP (BasicFunctor First) where
  (=<<) = (Prelude.=<<)

-- Sets

data SetP

type instance FunctorT SetP a = Set a
type instance FunctorSrcC SetP _ = ()
type instance FunctorDstC SetP b = (Ord (ToType b))
instance Functor FunctionP SetP where
  fmap = Data.Set.map
instance Pure SetP where
  pure = Data.Set.singleton
instance Apply FunctionP SetP where
  fs <*> x = Data.Set.fromList (Data.Set.toList fs <*> Data.Set.toList x)

data TupleP (n :: Nat)
type instance FunctorSrcC (TupleP _) _ = ()
type instance FunctorDstC (TupleP _) _ = ()

-- Tuple
type instance FunctorT (TupleP 2) a = (a,a)
instance Functor FunctionP (TupleP 2) where
  fmap f (x1,x2) = (f x1, f x2)
instance Pure (TupleP 2) where
  pure x = (x,x)
instance Apply FunctionP (TupleP 2) where
  (<*>) (f1,f2) (x1,x2) = (f1 x1, f2 x2)

type instance FunctorT (TupleP 3) a = (a,a,a)
instance Functor FunctionP (TupleP 3) where
  fmap f (x1,x2,x3) = (f x1, f x2, f x3)
instance Pure (TupleP 3) where
  pure x = (x,x,x)
instance Apply FunctionP (TupleP 3) where
  (<*>) (f1,f2,f3) (x1,x2,x3) = (f1 x1, f2 x2, f3 x3)

type instance FunctorT (TupleP 4) a = (a,a,a,a)
instance Functor FunctionP (TupleP 4) where
  fmap f (x1,x2,x3,x4) = (f x1, f x2, f x3, f x4)
instance Pure (TupleP 4) where
  pure x = (x,x,x,x)
instance Apply FunctionP (TupleP 4) where
  (<*>) (f1,f2,f3,f4) (x1,x2,x3,x4) = (f1 x1, f2 x2, f3 x3, f4 x4)

type instance FunctorT (TupleP 5) a = (a,a,a,a,a)
instance Functor FunctionP (TupleP 5) where
  fmap f (x1,x2,x3,x4,x5) = (f x1, f x2, f x3, f x4, f x5)
instance Pure (TupleP 5) where
  pure x = (x,x,x,x,x)
instance Apply FunctionP (TupleP 5) where
  (<*>) (f1,f2,f3,f4,f5) (x1,x2,x3,x4,x5) = (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5)
