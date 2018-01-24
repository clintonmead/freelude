{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Freelude.Impl.Category (
  Semigroupoid((.)), (<<<), (>>>),
  Category(id),
  Const(const),
  Arr(arr),
  FunctionP,
  CategoryT, ExoCategoryT,
  CategorySrcC, CategorySrcC', ExoCategorySrcC,
  CategoryDstC, CategoryDstC', ExoCategoryDstC,
  CategoryC, ExoCategoryC,
  IsSemigroupoid, ExoIsSemigroupoid,
  IsCategory, ExoIsCategory,
  FunctorT, FunctorSrcC, FunctorDstC, FunctorSrcC', FunctorDstC',
  FromMaybeConstraintFunc,
  UnconstrainedFunctor,
  BasicFunctorP, FunctorCategoryP,
  Functor(fmap), (<$>),
  ConstFunctor((<$)),
  Pure(pure),
  Lift(liftA2, (<*), (*>)),
  Apply((<*>)), (<**>),
  Applicative,
  Monad((>>=), (>>)), return, (=<<)
) where

import qualified Control.Category
import qualified Control.Applicative

import Data.Type.Equality ((:~:))
import Data.Type.Coercion (Coercion)
import Control.Arrow (Kleisli)
import Data.Monoid (Dual(Dual))
import qualified Control.Arrow

import Control.IndexT.Tuple (TupleConstraint)
import Control.IndexT (IndexT)
import Control.IndexT.Constructor (IndexC, IndexCK)

import Data.Functor.Identity (Identity(Identity))
import Prelude hiding (Functor(fmap), (<$>), Applicative((<*>), pure), Monad(return, (>>=), (>>)), (=<<), (.), id, const)
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

import Data.Array (Array, Ix)
import qualified Data.Array.IArray
import Data.Array.IArray (IArray)
import Data.Array.Unboxed (UArray)

import qualified Data.Text as StrictText
import qualified Data.Text.Lazy as LazyText

import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Lazy as LazyByteString

import Data.Word (Word8)

type family CategoryT (p :: Type) (a :: Type) (b :: Type) = (f :: Type) | f -> p a b
type family ExoCategoryT (p :: Type) (a :: Type) (b :: Type) = (f :: Type) | f -> p a b
type family CategorySrcC' (p :: Type) :: Maybe (Type -> Constraint)
type family CategoryDstC' (p :: Type) :: Maybe (Type -> Constraint)
type family ExoCategorySrcC (p :: Type) (a :: Type) :: Constraint
type family ExoCategoryDstC (p :: Type) (b :: Type) :: Constraint

class EmptyConstraint a
instance EmptyConstraint a

type family FromMaybeConstraintFunc (p :: Maybe (Type -> Constraint)) :: Type -> Constraint where
  FromMaybeConstraintFunc 'Nothing = EmptyConstraint
  FromMaybeConstraintFunc ('Just c) = c

type CategorySrcC p a = (FromMaybeConstraintFunc (CategorySrcC' p)) a
type CategoryDstC p a = (FromMaybeConstraintFunc (CategoryDstC' p)) a

type CategoryC p a b = (CategorySrcC p a, CategoryDstC p b)
type ExoCategoryC p a b = (ExoCategorySrcC p a, ExoCategoryDstC p b)
type IsSemigroupoid t p a b = (Semigroupoid p, t ~ CategoryT p a b, CategoryC p a b)
type ExoIsSemigroupoid t p a b = (Semigroupoid p, t ~ ExoCategoryT p a b, ExoCategoryC p a b)
type IsCategory t p a b = (IsSemigroupoid t p a b, Category p)
type ExoIsCategory t p a b = (ExoIsSemigroupoid t p a b, Category p)

class Semigroupoid p where
  (.) :: (CategoryC p b c, CategoryC p a b, CategoryC p a c) => CategoryT p b c -> CategoryT p a b -> CategoryT p a c

class Semigroupoid p => Category p where
  id :: (CategoryC p a a, ExoCategoryC p a a, t ~ CategoryT p a a, t ~ ExoCategoryT p a a) => t

class Semigroupoid p => Const p where
  const :: CategoryC p a b => b -> CategoryT p a b
  default const :: (Arr p, CategoryC p a b) => b -> CategoryT p a b
  const = arr Prelude.. Prelude.const
-- * Instances from 'Control.Category'
-- ** '->'
instance {-# OVERLAPPABLE #-} (Semigroupoid p, Arr p) => Const p

class (Category p, Const p) => Arr p where
  arr :: CategoryC p a b => (a -> b) -> CategoryT p a b


data FunctionP

type instance CategoryT FunctionP a b = (->) a b
type instance ExoCategoryT FunctionP a b = (->) a b
type instance CategorySrcC' FunctionP = 'Nothing
type instance CategoryDstC' FunctionP = 'Nothing
type instance ExoCategorySrcC FunctionP a = ()
type instance ExoCategoryDstC FunctionP b = ()

infixr 9  .
instance Semigroupoid FunctionP where
  (.) = (Prelude..)
instance Category FunctionP where
  id = Prelude.id
instance Const FunctionP
instance Arr FunctionP where
  arr = id

(<<<) :: (Semigroupoid p, CategoryC p b c, CategoryC p a b, CategoryC p a c) => CategoryT p b c -> CategoryT p a b -> CategoryT p a c
(<<<) = (.)

(>>>) :: (Semigroupoid p, CategoryC p a b, CategoryC p b c, CategoryC p a c) => CategoryT p a b -> CategoryT p b c -> CategoryT p a c
(>>>) = flip (.)

-- ** 'Data.Equality.:-:'
data ProxyK k (a :: k)
data TypeEqP (k :: Type)

class (a ~ ProxyK k (IndexCK k 2 1 a)) => ProxyC k a
instance (a ~ ProxyK k (IndexCK k 2 1 a)) => ProxyC k a

type instance CategoryT (TypeEqP k) (ProxyK k a) (ProxyK k b) = (:~:) a b
type instance ExoCategoryT (TypeEqP k) (ProxyK k a) (ProxyK k b) = (:~:) a b
type instance CategorySrcC' (TypeEqP k) = 'Just (ProxyC k)
type instance CategoryDstC' (TypeEqP k) = 'Just (ProxyC k)
type instance ExoCategorySrcC (TypeEqP k) a = CategorySrcC (TypeEqP k) a
type instance ExoCategoryDstC (TypeEqP k) a = CategoryDstC (TypeEqP k) a

instance Semigroupoid (TypeEqP k) where
  (.) = (Control.Category..)
instance Category (TypeEqP k) where
  id = Control.Category.id

data CoercionP (k :: Type)

type instance CategoryT (CoercionP k) (ProxyK k a) (ProxyK k b) = Coercion a b
type instance CategorySrcC' (CoercionP k) = 'Just (ProxyC k)
type instance CategoryDstC' (CoercionP k) = 'Just (ProxyC k)
type instance ExoCategoryT (CoercionP k) (ProxyK k a) (ProxyK k b) = Coercion a b
type instance ExoCategorySrcC (CoercionP k) a = CategorySrcC (TypeEqP k) a
type instance ExoCategoryDstC (CoercionP k) a = CategoryDstC (TypeEqP k) a

instance Semigroupoid (CoercionP k) where
  (.) = (Control.Category..)
instance Category (CoercionP k) where
  id = Control.Category.id

-- ** 'Control.Category.Kleisli'

data KleisliP (m :: Type -> Type)

type instance CategoryT (KleisliP m) a b = Kleisli m a b
type instance CategorySrcC' (KleisliP _) = 'Nothing
type instance CategoryDstC' (KleisliP _) = 'Nothing
type instance ExoCategoryT (KleisliP m) (m a) (m b) = Kleisli m a b
type instance ExoCategorySrcC (KleisliP _) _ = ()
type instance ExoCategoryDstC (KleisliP _) _ = ()

instance Prelude.Monad m => Semigroupoid (KleisliP m)  where
  (.) = (Control.Category..)
instance Prelude.Monad m => Category (KleisliP m) where
  id = Control.Category.id
instance Prelude.Monad m => Const (KleisliP m)
instance Prelude.Monad m => Arr (KleisliP m) where
  arr = Control.Arrow.arr
-- * Data.Semigroup

data FunctorCategoryP (functorP :: Type) (p :: Type)

type instance CategorySrcC' (FunctorCategoryP _ p) = CategorySrcC' p
type instance CategoryDstC' (FunctorCategoryP _ p) = CategoryDstC' p
type instance ExoCategorySrcC (FunctorCategoryP functorP p) a = (ExoCategorySrcC p (IndexC 1 0 a), a ~ FunctorT functorP (IndexC 1 0 a))
type instance ExoCategoryDstC (FunctorCategoryP functorP p) b = (ExoCategoryDstC p (IndexC 1 0 b), b ~ FunctorT functorP (IndexC 1 0 b))

-- ** 'Maybe'
type instance CategoryT (FunctorCategoryP (BasicFunctorP Maybe) p) a b = Maybe (CategoryT p a b)
type instance ExoCategoryT (FunctorCategoryP (BasicFunctorP Maybe) p) (Maybe a) (Maybe b) = Maybe (ExoCategoryT p a b)

instance Semigroupoid p => Semigroupoid (FunctorCategoryP (BasicFunctorP Maybe) p) where
  x . y = (.) <$> x <*> y
instance Semigroupoid p => Category (FunctorCategoryP (BasicFunctorP Maybe) p) where
  id = Nothing
instance Const (FunctorCategoryP (BasicFunctorP Maybe) FunctionP)
instance Arr (FunctorCategoryP (BasicFunctorP Maybe) FunctionP) where
  arr = pure

-- ** Lists

type instance CategoryT (FunctorCategoryP (BasicFunctorP []) p) a b = [CategoryT p a b]
type instance ExoCategoryT (FunctorCategoryP (BasicFunctorP []) p) [a] [b] = [ExoCategoryT p a b]

instance Semigroupoid p => Semigroupoid (FunctorCategoryP (BasicFunctorP []) p) where
  x . y = (.) <$> x <*> y
instance Semigroupoid p => Category (FunctorCategoryP (BasicFunctorP []) p) where
  id = mempty
instance Const (FunctorCategoryP (BasicFunctorP []) FunctionP) where
instance Arr (FunctorCategoryP (BasicFunctorP []) FunctionP) where
  arr = pure

-- ** Identity

type instance CategoryT (Identity p) a b = Identity (CategoryT p a b)
type instance CategorySrcC' (Identity p) = CategorySrcC' p
type instance CategoryDstC' (Identity p) = CategoryDstC' p
type instance ExoCategoryT (Identity p) a b = Identity (ExoCategoryT p a b)
type instance ExoCategorySrcC (Identity p) a = ExoCategorySrcC p a
type instance ExoCategoryDstC (Identity p) b = ExoCategoryDstC p b

instance Semigroupoid p => Semigroupoid (Identity p) where
  x . y = (.) <$> x <*> y

instance Category p => Category (Identity p) where
  id = Identity id

instance Const (Identity FunctionP)
instance Arr (Identity FunctionP) where
  arr = pure

-- ** Dual

type instance CategoryT (Dual p) a b = Dual (CategoryT p b a)
type instance CategorySrcC' (Dual p) = CategoryDstC' p
type instance CategoryDstC' (Dual p) = CategorySrcC' p
type instance ExoCategoryT (Dual p) a b = Dual (ExoCategoryT p b a)
type instance ExoCategorySrcC (Dual p) a = ExoCategoryDstC p a
type instance ExoCategoryDstC (Dual p) b = ExoCategorySrcC p b

instance Semigroupoid p => Semigroupoid (Dual p) where
  Dual x . Dual y = Dual (y . x)

instance Category p => Category (Dual p) where
  id = Dual id

-- ** Tuples

class (TupleConstraint 2 a, CategorySrcC p1 (IndexT 0 a), CategorySrcC p2 (IndexT 1 a)) => Tuple2SrcC p1 p2 a
instance (TupleConstraint 2 a, CategorySrcC p1 (IndexT 0 a), CategorySrcC p2 (IndexT 1 a)) => Tuple2SrcC p1 p2 a

class (TupleConstraint 2 b, CategoryDstC p1 (IndexT 0 b), CategoryDstC p2 (IndexT 1 b)) => Tuple2DstC p1 p2 b
instance (TupleConstraint 2 b, CategoryDstC p1 (IndexT 0 b), CategoryDstC p2 (IndexT 1 b)) => Tuple2DstC p1 p2 b

type instance CategoryT (p1, p2) (a1, a2) (b1, b2) = (CategoryT p1 a1 b1, CategoryT p2 a2 b2)
type instance CategorySrcC' (p1, p2) = 'Just (Tuple2SrcC p1 p2)
type instance CategoryDstC' (p1, p2) = 'Just (Tuple2DstC p1 p2)
type instance ExoCategoryT (p1, p2) (a1, a2) (b1, b2) = (ExoCategoryT p1 a1 b1, ExoCategoryT p2 a2 b2)
type instance ExoCategorySrcC (p1, p2) a = (TupleConstraint 2 a, ExoCategorySrcC p1 (IndexT 0 a), ExoCategorySrcC p2 (IndexT 1 a))
type instance ExoCategoryDstC (p1, p2) b = (TupleConstraint 2 b, ExoCategoryDstC p1 (IndexT 0 b), ExoCategoryDstC p2 (IndexT 1 b))

instance (Semigroupoid p1, Semigroupoid p2) => Semigroupoid (p1, p2) where
  (x1, x2) . (y1, y2) = (x1 . y1, x2 . y2)

class (TupleConstraint 3 a, CategorySrcC p1 (IndexT 0 a), CategorySrcC p2 (IndexT 1 a), CategorySrcC p3 (IndexT 2 a)) => Tuple3SrcC p1 p2 p3 a
instance (TupleConstraint 3 a, CategorySrcC p1 (IndexT 0 a), CategorySrcC p2 (IndexT 1 a), CategorySrcC p3 (IndexT 2 a)) => Tuple3SrcC p1 p2 p3 a

class (TupleConstraint 3 b, CategoryDstC p1 (IndexT 0 b), CategoryDstC p2 (IndexT 1 b), CategoryDstC p3 (IndexT 2 b)) => Tuple3DstC p1 p2 p3 b
instance (TupleConstraint 3 b, CategoryDstC p1 (IndexT 0 b), CategoryDstC p2 (IndexT 1 b), CategoryDstC p3 (IndexT 2 b)) => Tuple3DstC p1 p2 p3 b

type instance CategoryT (p1, p2, p3) (a1, a2, a3) (b1, b2, b3) = (CategoryT p1 a1 b1, CategoryT p2 a2 b2, CategoryT p3 a3 b3)
type instance CategorySrcC' (p1, p2, p3) = 'Just (Tuple3SrcC p1 p2 p3)
type instance CategoryDstC' (p1, p2, p3) = 'Just (Tuple3DstC p1 p2 p3)
type instance ExoCategoryT (p1, p2, p3) (a1, a2, a3) (b1, b2, b3) = (ExoCategoryT p1 a1 b1, ExoCategoryT p2 a2 b2, ExoCategoryT p3 a3 b3)
type instance ExoCategorySrcC (p1, p2, p3) a = (TupleConstraint 3 a, ExoCategorySrcC p1 (IndexT 0 a), ExoCategorySrcC p2 (IndexT 1 a), ExoCategorySrcC p3 (IndexT 2 a))
type instance ExoCategoryDstC (p1, p2, p3) b = (TupleConstraint 3 b, ExoCategoryDstC p1 (IndexT 0 b), ExoCategoryDstC p2 (IndexT 1 b), ExoCategoryDstC p3 (IndexT 2 b))

instance (Semigroupoid p1, Semigroupoid p2, Semigroupoid p3) => Semigroupoid (p1, p2, p3) where
  (x1, x2, x3) . (y1, y2, y3) = (x1 . y1, x2 . y2, x3 . y3)

class (TupleConstraint 4 a, CategorySrcC p1 (IndexT 0 a), CategorySrcC p2 (IndexT 1 a), CategorySrcC p3 (IndexT 2 a), CategorySrcC p4 (IndexT 3 a)) => Tuple4SrcC p1 p2 p3 p4 a
instance (TupleConstraint 4 a, CategorySrcC p1 (IndexT 0 a), CategorySrcC p2 (IndexT 1 a), CategorySrcC p3 (IndexT 2 a), CategorySrcC p4 (IndexT 3 a)) => Tuple4SrcC p1 p2 p3 p4 a

class (TupleConstraint 4 b, CategoryDstC p1 (IndexT 0 b), CategoryDstC p2 (IndexT 1 b), CategoryDstC p3 (IndexT 2 b), CategoryDstC p4 (IndexT 3 b)) => Tuple4DstC p1 p2 p3 p4 b
instance (TupleConstraint 4 b, CategoryDstC p1 (IndexT 0 b), CategoryDstC p2 (IndexT 1 b), CategoryDstC p3 (IndexT 2 b), CategoryDstC p4 (IndexT 3 b)) => Tuple4DstC p1 p2 p3 p4 b

type instance CategoryT (p1, p2, p3, p4) (a1, a2, a3, a4) (b1, b2, b3, b4) = (CategoryT p1 a1 b1, CategoryT p2 a2 b2, CategoryT p3 a3 b3, CategoryT p4 a4 b4)
type instance CategorySrcC' (p1, p2, p3, p4) = 'Just (Tuple4SrcC p1 p2 p3 p4)
type instance CategoryDstC' (p1, p2, p3, p4) = 'Just (Tuple4DstC p1 p2 p3 p4)
type instance ExoCategoryT (p1, p2, p3, p4) (a1, a2, a3, a4) (b1, b2, b3, b4) = (ExoCategoryT p1 a1 b1, ExoCategoryT p2 a2 b2, ExoCategoryT p3 a3 b3, ExoCategoryT p4 a4 b4)
type instance ExoCategorySrcC (p1, p2, p3, p4) a = (TupleConstraint 4 a, ExoCategorySrcC p1 (IndexT 0 a), ExoCategorySrcC p2 (IndexT 1 a), ExoCategorySrcC p3 (IndexT 2 a), ExoCategorySrcC p4 (IndexT 3 a))
type instance ExoCategoryDstC (p1, p2, p3, p4) b = (TupleConstraint 4 b, ExoCategoryDstC p1 (IndexT 0 b), ExoCategoryDstC p2 (IndexT 1 b), ExoCategoryDstC p3 (IndexT 2 b), ExoCategoryDstC p4 (IndexT 3 b))

instance (Semigroupoid p1, Semigroupoid p2, Semigroupoid p3, Semigroupoid p4) => Semigroupoid (p1, p2, p3, p4) where
  (x1, x2, x3, x4) . (y1, y2, y3, y4) = (x1 . y1, x2 . y2, x3 . y3, x4 . y4)

class (TupleConstraint 5 a, CategorySrcC p1 (IndexT 0 a), CategorySrcC p2 (IndexT 1 a), CategorySrcC p3 (IndexT 2 a), CategorySrcC p4 (IndexT 3 a), CategorySrcC p5 (IndexT 4 a)) => Tuple5SrcC p1 p2 p3 p4 p5 a
instance (TupleConstraint 5 a, CategorySrcC p1 (IndexT 0 a), CategorySrcC p2 (IndexT 1 a), CategorySrcC p3 (IndexT 2 a), CategorySrcC p4 (IndexT 3 a), CategorySrcC p5 (IndexT 4 a)) => Tuple5SrcC p1 p2 p3 p4 p5 a

class (TupleConstraint 5 b, CategoryDstC p1 (IndexT 0 b), CategoryDstC p2 (IndexT 1 b), CategoryDstC p3 (IndexT 2 b), CategoryDstC p4 (IndexT 3 b), CategoryDstC p5 (IndexT 4 b)) => Tuple5DstC p1 p2 p3 p4 p5 b
instance (TupleConstraint 5 b, CategoryDstC p1 (IndexT 0 b), CategoryDstC p2 (IndexT 1 b), CategoryDstC p3 (IndexT 2 b), CategoryDstC p4 (IndexT 3 b), CategoryDstC p5 (IndexT 4 b)) => Tuple5DstC p1 p2 p3 p4 p5 b

type instance CategoryT (p1, p2, p3, p4, p5) (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5) = (CategoryT p1 a1 b1, CategoryT p2 a2 b2, CategoryT p3 a3 b3, CategoryT p4 a4 b4, CategoryT p5 a5 b5)
type instance CategorySrcC' (p1, p2, p3, p4, p5) = 'Just (Tuple5SrcC p1 p2 p3 p4 p5)
type instance CategoryDstC' (p1, p2, p3, p4, p5) = 'Just (Tuple5DstC p1 p2 p3 p4 p5)
type instance ExoCategoryT (p1, p2, p3, p4, p5) (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5) = (ExoCategoryT p1 a1 b1, ExoCategoryT p2 a2 b2, ExoCategoryT p3 a3 b3, ExoCategoryT p4 a4 b4, ExoCategoryT p5 a5 b5)
type instance ExoCategorySrcC (p1, p2, p3, p4, p5) a = (TupleConstraint 5 a, ExoCategorySrcC p1 (IndexT 0 a), ExoCategorySrcC p2 (IndexT 1 a), ExoCategorySrcC p3 (IndexT 2 a), ExoCategorySrcC p4 (IndexT 3 a), ExoCategorySrcC p5 (IndexT 4 a))
type instance ExoCategoryDstC (p1, p2, p3, p4, p5) b = (TupleConstraint 5 b, ExoCategoryDstC p1 (IndexT 0 b), ExoCategoryDstC p2 (IndexT 1 b), ExoCategoryDstC p3 (IndexT 2 b), ExoCategoryDstC p4 (IndexT 3 b), ExoCategoryDstC p5 (IndexT 4 b))

instance (Semigroupoid p1, Semigroupoid p2, Semigroupoid p3, Semigroupoid p4, Semigroupoid p5) => Semigroupoid (p1, p2, p3, p4, p5) where
  (x1, x2, x3, x4, x5) . (y1, y2, y3, y4, y5) = (x1 . y1, x2 . y2, x3 . y3, x4 . y4, x5 . y5)

-- Functor

type family FunctorT (p :: Type) (a :: Type) = (b :: Type) | b -> p a

type family FunctorSrcC' (p :: Type) :: Maybe (Type -> Constraint)
type family FunctorDstC' (p :: Type) :: Maybe (Type -> Constraint)

type FunctorSrcC p a = FromMaybeConstraintFunc (FunctorSrcC' p) a
type FunctorDstC p a = FromMaybeConstraintFunc (FunctorDstC' p) a

class Semigroupoid cat => Functor cat p where
  fmap ::
    (ra ~ FunctorT p a, rb ~ FunctorT p b, CategoryC cat a b, CategoryC cat ra rb, FunctorSrcC p a, FunctorDstC p b) =>
    CategoryT cat a b -> CategoryT cat ra rb

  default fmap ::
    (Lift p, Pure p, ra ~ FunctorT p a, rb ~ FunctorT p b, CategoryC cat a b, CategoryC cat ra rb, FunctorSrcC p a, FunctorDstC p b, cat ~ FunctionP, FunctorSrcC' p ~ 'Nothing, FunctorDstC' p ~ 'Nothing) =>
    CategoryT cat a b -> CategoryT cat ra rb
  fmap f x = liftA2 (const f) (pure x) x

type UnconstrainedFunctor cat p = (Functor cat p, FunctorSrcC' p ~ 'Nothing, FunctorDstC' p ~ 'Nothing)

infixl 4 <$>
(<$>) ::
  (Functor cat p, ra ~ FunctorT p a, rb ~ FunctorT p b, CategoryC cat a b, CategoryC cat ra rb, FunctorSrcC p a, FunctorDstC p b) =>
  CategoryT cat a b -> CategoryT cat ra rb
(<$>) = fmap

infixl 4 <$
class Functor cat p => ConstFunctor cat p where
  (<$) ::
    (ra ~ FunctorT p a, rb ~ FunctorT p b, CategoryC cat a b, CategoryC cat ra rb, FunctorSrcC p a, FunctorDstC p b) =>
    b -> CategoryT cat ra rb
  default (<$) ::
    (Const cat, ra ~ FunctorT p a, rb ~ FunctorT p b, CategoryC cat a b, CategoryC cat ra rb, FunctorSrcC p a, FunctorDstC p b) =>
    b -> CategoryT cat ra rb
  (<$) = fmap . const

instance {-# OVERLAPPABLE #-} (Functor cat p, Const cat) => ConstFunctor cat p

infixl 4 <*>, <*, *>, <**>

class Functor FunctionP p => Lift p where
  liftA2 ::
    (FunctorSrcC p a, FunctorSrcC p b, FunctorDstC p c) =>
    (a -> b -> c) -> FunctorT p a -> FunctorT p b -> FunctorT p c
  default liftA2 ::
    (Monad p, Pure p, FunctorSrcC' p ~ 'Nothing, FunctorDstC' p ~ 'Nothing) =>
    (a -> b -> c) -> FunctorT p a -> FunctorT p b -> FunctorT p c
  liftA2 f x y = (pure f >>= g x) >>= g y where
    g x' y' = x' >>= (pure . y')
  (*>) :: (FunctorSrcC p a, FunctorSrcC p b, FunctorDstC p b) => FunctorT p a -> FunctorT p b -> FunctorT p b
  (*>) = liftA2 (flip const)
  (<*) :: (FunctorSrcC p a, FunctorSrcC p b, FunctorDstC p a) => FunctorT p a -> FunctorT p b -> FunctorT p a
  (<*) = liftA2 const

class Lift p => Apply p where
  (<*>) :: FunctorT p (a -> b) -> FunctorT p a -> FunctorT p b
  default (<*>) :: (FunctorSrcC' p ~ 'Nothing, FunctorDstC' p ~ 'Nothing) => FunctorT p (a -> b) -> FunctorT p a -> FunctorT p b
  (<*>) = liftA2 id

(<**>) :: Apply p => FunctorT p a -> FunctorT p (a -> b) -> FunctorT p b
(<**>) = flip (<*>)

class Pure p where
  pure :: (FunctorDstC p a) => a -> FunctorT p a

type Applicative p = (Apply p, Pure p)

infixl 1 >>, >>=
class (Lift p, Pure p) => Monad p where
  (>>=) :: (FunctorSrcC p a, FunctorSrcC p b, FunctorDstC p b) => FunctorT p a -> (a -> FunctorT p b) -> FunctorT p b
  (>>) :: (FunctorSrcC p a, FunctorSrcC p b, FunctorDstC p b) => FunctorT p a -> FunctorT p b -> FunctorT p b
  m >> k = m >>= Prelude.const k

return :: (Monad p, FunctorDstC p a) => a -> FunctorT p a
return = pure

infixr 1  =<<
(=<<) :: (Monad p, FunctorSrcC p a, FunctorSrcC p b, FunctorDstC p b) => (a -> FunctorT p b) -> FunctorT p a -> FunctorT p b
(=<<) = flip (>>=)


data BasicFunctorP f
type instance FunctorSrcC' (BasicFunctorP _) = 'Nothing
type instance FunctorDstC' (BasicFunctorP _) = 'Nothing

data ConstantP (a :: Type)
type instance FunctorT (BasicFunctorP (ConstantP a)) b = Constant a b
instance Functor FunctionP (BasicFunctorP (ConstantP a)) where
  fmap = Prelude.fmap

type instance FunctorT (BasicFunctorP Maybe) a = Maybe a
instance Functor FunctionP (BasicFunctorP Maybe) where
  fmap = Prelude.fmap
instance Pure (BasicFunctorP Maybe) where
  pure = Prelude.pure
instance Lift (BasicFunctorP Maybe) where
  liftA2 = Control.Applicative.liftA2
instance Apply (BasicFunctorP Maybe) where
  (<*>) = (Prelude.<*>)
instance Monad (BasicFunctorP Maybe) where
  (>>=) = (Prelude.>>=)
  (>>) = (Prelude.>>)

type instance FunctorT (BasicFunctorP Identity) a = Identity a
instance Functor FunctionP (BasicFunctorP Identity) where
  fmap = Prelude.fmap
instance Pure (BasicFunctorP Identity) where
  pure = Prelude.pure
instance Lift (BasicFunctorP Identity) where
  liftA2 = Control.Applicative.liftA2
instance Apply (BasicFunctorP Identity) where
  (<*>) = (Prelude.<*>)
instance Monad (BasicFunctorP Identity) where
  (>>=) = (Prelude.>>=)
  (>>) = (Prelude.>>)

type instance FunctorT (BasicFunctorP NonEmpty) a = NonEmpty a
instance Functor FunctionP (BasicFunctorP NonEmpty) where
  fmap = Prelude.fmap
instance Pure (BasicFunctorP NonEmpty) where
  pure = Prelude.pure
instance Lift (BasicFunctorP NonEmpty) where
  liftA2 = Control.Applicative.liftA2
instance Apply (BasicFunctorP NonEmpty) where
  (<*>) = (Prelude.<*>)
instance Monad (BasicFunctorP NonEmpty) where
  (>>=) = (Prelude.>>=)
  (>>) = (Prelude.>>)

type instance FunctorT (BasicFunctorP (Either a)) b = Either a b
instance Functor FunctionP (BasicFunctorP (Either a)) where
  fmap = Prelude.fmap
instance Pure (BasicFunctorP (Either a)) where
  pure = Prelude.pure
instance Lift (BasicFunctorP (Either a)) where
  liftA2 = Control.Applicative.liftA2
instance Apply (BasicFunctorP (Either a)) where
  (<*>) = (Prelude.<*>)
instance Monad (BasicFunctorP (Either a)) where
  (>>=) = (Prelude.>>=)
  (>>) = (Prelude.>>)

type instance FunctorT (BasicFunctorP []) a = [a]
instance Functor FunctionP (BasicFunctorP []) where
  fmap = Prelude.fmap
instance Pure (BasicFunctorP []) where
  pure = Prelude.pure
instance Lift (BasicFunctorP []) where
  liftA2 = Control.Applicative.liftA2
instance Apply (BasicFunctorP []) where
  (<*>) = (Prelude.<*>)
instance Monad (BasicFunctorP []) where
  (>>=) = (Prelude.>>=)
  (>>) = (Prelude.>>)

type instance FunctorT (BasicFunctorP IO) a = IO a
instance Functor FunctionP (BasicFunctorP IO) where
  fmap = Prelude.fmap
instance Pure (BasicFunctorP IO) where
  pure = Prelude.pure
instance Lift (BasicFunctorP IO) where
  liftA2 = Control.Applicative.liftA2
instance Apply (BasicFunctorP IO) where
  (<*>) = (Prelude.<*>)
instance Monad (BasicFunctorP IO) where
  (>>=) = (Prelude.>>=)
  (>>) = (Prelude.>>)

type instance FunctorT (BasicFunctorP Option) a = Option a
instance Functor FunctionP (BasicFunctorP Option) where
  fmap = Prelude.fmap
instance Pure (BasicFunctorP Option) where
  pure = Prelude.pure
instance Lift (BasicFunctorP Option) where
  liftA2 = Control.Applicative.liftA2
instance Apply (BasicFunctorP Option) where
  (<*>) = (Prelude.<*>)
instance Monad (BasicFunctorP Option) where
  (>>=) = (Prelude.>>=)
  (>>) = (Prelude.>>)

type instance FunctorT (BasicFunctorP Tree) a = Tree a
instance Functor FunctionP (BasicFunctorP Tree) where
  fmap = Prelude.fmap
instance Pure (BasicFunctorP Tree) where
  pure = Prelude.pure
instance Lift (BasicFunctorP Tree) where
  liftA2 = Control.Applicative.liftA2
instance Apply (BasicFunctorP Tree) where
  (<*>) = (Prelude.<*>)
instance Monad (BasicFunctorP Tree) where
  (>>=) = (Prelude.>>=)
  (>>) = (Prelude.>>)

type instance FunctorT (BasicFunctorP Min) a = Min a
instance Functor FunctionP (BasicFunctorP Min) where
  fmap = Prelude.fmap
instance Pure (BasicFunctorP Min) where
  pure = Prelude.pure
instance Lift (BasicFunctorP Min) where
  liftA2 = Control.Applicative.liftA2
instance Apply (BasicFunctorP Min) where
  (<*>) = (Prelude.<*>)
instance Monad (BasicFunctorP Min) where
  (>>=) = (Prelude.>>=)
  (>>) = (Prelude.>>)

type instance FunctorT (BasicFunctorP Max) a = Max a
instance Functor FunctionP (BasicFunctorP Max) where
  fmap = Prelude.fmap
instance Pure (BasicFunctorP Max) where
  pure = Prelude.pure
instance Lift (BasicFunctorP Max) where
  liftA2 = Control.Applicative.liftA2
instance Apply (BasicFunctorP Max) where
  (<*>) = (Prelude.<*>)
instance Monad (BasicFunctorP Max) where
  (>>=) = (Prelude.>>=)
  (>>) = (Prelude.>>)

type instance FunctorT (BasicFunctorP Last) a = Last a
instance Functor FunctionP (BasicFunctorP Last) where
  fmap = Prelude.fmap
instance Pure (BasicFunctorP Last) where
  pure = Prelude.pure
instance Lift (BasicFunctorP Last) where
  liftA2 = Control.Applicative.liftA2
instance Apply (BasicFunctorP Last) where
  (<*>) = (Prelude.<*>)
instance Monad (BasicFunctorP Last) where
  (>>=) = (Prelude.>>=)
  (>>) = (Prelude.>>)

type instance FunctorT (BasicFunctorP First) a = First a
instance Functor FunctionP (BasicFunctorP First) where
  fmap = Prelude.fmap
instance Pure (BasicFunctorP First) where
  pure = Prelude.pure
instance Lift (BasicFunctorP First) where
  liftA2 = Control.Applicative.liftA2
instance Apply (BasicFunctorP First) where
  (<*>) = (Prelude.<*>)
instance Monad (BasicFunctorP First) where
  (>>=) = (Prelude.>>=)
  (>>) = (Prelude.>>)

data StrictOrLazyP = StrictP | LazyP

data TextP (p :: StrictOrLazyP)
type instance FunctorSrcC' (TextP _) = 'Just ((~) Char)
type instance FunctorDstC' (TextP _) = 'Just ((~) Char)

type instance FunctorT (TextP 'StrictP) Char = StrictText.Text
instance Functor FunctionP (TextP 'StrictP) where
  fmap = StrictText.map
instance Pure (TextP 'StrictP) where
  pure = StrictText.singleton
instance Lift (TextP 'StrictP) where
  -- Just go to and from lists. Slow but correct
  -- This can probably be improved
  liftA2 f x y = StrictText.pack (liftA2 f (StrictText.unpack x) (StrictText.unpack y))
instance Monad (TextP 'StrictP) where
  -- Also going through lists. I'm quite sure this can be improved.
  x >>= f = StrictText.pack (StrictText.unpack x >>= (StrictText.unpack . f))

type instance FunctorT (TextP 'LazyP) Char = LazyText.Text
instance Functor FunctionP (TextP 'LazyP) where
  fmap = LazyText.map
instance Pure (TextP 'LazyP) where
  pure = LazyText.singleton
instance Lift (TextP 'LazyP) where
  -- Just go to and from lists. Slow but correct
  -- This can probably be improved
  liftA2 f x y = LazyText.pack (liftA2 f (LazyText.unpack x) (LazyText.unpack y))
instance Monad (TextP 'LazyP) where
  -- Also going through lists. I'm quite sure this can be improved.
  x >>= f = LazyText.pack (LazyText.unpack x >>= (LazyText.unpack . f))

data ByteStringP (p :: StrictOrLazyP)
type instance FunctorSrcC' (ByteStringP _) = 'Just ((~) Word8)
type instance FunctorDstC' (ByteStringP _) = 'Just ((~) Word8)

type instance FunctorT (ByteStringP 'StrictP) Word8 = StrictByteString.ByteString
instance Functor FunctionP (ByteStringP 'StrictP) where
  fmap = StrictByteString.map
instance Pure (ByteStringP 'StrictP) where
  pure = StrictByteString.singleton
instance Lift (ByteStringP 'StrictP) where
  -- Just go to and from lists. Slow but correct
  -- This can probably be improved
  liftA2 f x y = StrictByteString.pack (liftA2 f (StrictByteString.unpack x) (StrictByteString.unpack y))
instance Monad (ByteStringP 'StrictP) where
  -- Also going through lists. I'm quite sure this can be improved.
  x >>= f = StrictByteString.pack (StrictByteString.unpack x >>= (StrictByteString.unpack . f))

type instance FunctorT (ByteStringP 'LazyP) Word8 = LazyByteString.ByteString
instance Functor FunctionP (ByteStringP 'LazyP) where
  fmap = LazyByteString.map
instance Pure (ByteStringP 'LazyP) where
  pure = LazyByteString.singleton
instance Lift (ByteStringP 'LazyP) where
  -- Just go to and from lists. Slow but correct
  -- This can probably be improved
  liftA2 f x y = LazyByteString.pack (liftA2 f (LazyByteString.unpack x) (LazyByteString.unpack y))
instance Monad (ByteStringP 'LazyP) where
  -- Also going through lists. I'm quite sure this can be improved.
  x >>= f = LazyByteString.pack (LazyByteString.unpack x >>= (LazyByteString.unpack . f))

-- Arrays

arrayFmap :: (IArray a e1, IArray a e2, Ix i) => (e1 -> e2) -> a i e1 -> a i e2
arrayFmap = Data.Array.IArray.amap

arrayPure :: (IArray a e, Ix i, Num i) => e -> a i e
arrayPure x = Data.Array.IArray.listArray (0,0) [x]

arrayLiftA2 :: (IArray a e1, IArray a e2, IArray a e3, Ix i, Num i) => (e1 -> e2 -> e3) -> a i e1 -> a i e2 -> a i e3
arrayLiftA2 f x y = Data.Array.IArray.array (lbound, ubound) [(index_f x_i y_i, f x_e y_e) | (x_i,x_e) <- Data.Array.IArray.assocs x, (y_i, y_e) <- Data.Array.IArray.assocs y] where
  (x_l, x_u) = Data.Array.IArray.bounds x
  (y_l, y_u) = Data.Array.IArray.bounds y
  step = y_u - y_l + 1
  index_f x_i y_i = x_i * step + y_i
  lbound = index_f x_l y_l
  ubound = index_f x_u y_u

type instance FunctorT (BasicFunctorP (Array i)) e = Array i e
instance (Ix i) => Functor FunctionP (BasicFunctorP (Array i)) where
  fmap = arrayFmap
instance (Ix i, Num i) => Pure (BasicFunctorP (Array i)) where
  pure = arrayPure
instance (Ix i, Num i) => Lift (BasicFunctorP (Array i)) where
  liftA2 = arrayLiftA2

data UArrayP (indexT :: Type)

class (IArray UArray e) => UArrayC e
instance (IArray UArray e) => UArrayC e

type instance FunctorT (UArrayP i) e = UArray i e
type instance FunctorSrcC' (UArrayP i) = 'Just UArrayC
type instance FunctorDstC' (UArrayP i) = 'Just UArrayC

instance (Ix i) => Functor FunctionP (UArrayP i) where
  fmap = arrayFmap
instance (Ix i, Num i) => Pure (UArrayP i) where
  pure = arrayPure
instance (Ix i, Num i) => Lift (UArrayP i) where
  liftA2 = arrayLiftA2

-- Sets

data SetP

class (Ord (ToType b)) => SetDstC b
instance (Ord (ToType b)) => SetDstC b

type instance FunctorT SetP a = Set a
type instance FunctorSrcC' SetP = 'Nothing
type instance FunctorDstC' SetP = 'Just SetDstC

instance Functor FunctionP SetP where
  fmap = Data.Set.map
instance Pure SetP where
  pure = Data.Set.singleton
instance Lift SetP where
  liftA2 f x y = Data.Set.fromList (Control.Applicative.liftA2 f (Data.Set.toList x) (Data.Set.toList y))
instance Monad SetP where
  x >>= f = Data.Set.fromList (Data.Set.toList x >>= (Data.Set.toList . f))

data TupleP (n :: Nat)
type instance FunctorSrcC' (TupleP _) = 'Nothing
type instance FunctorDstC' (TupleP _) = 'Nothing

-- Tuple
type instance FunctorT (TupleP 2) a = (a,a)
instance Functor FunctionP (TupleP 2) where
  fmap f (x1,x2) = (f x1, f x2)
instance Pure (TupleP 2) where
  pure x = (x,x)
instance Lift (TupleP 2) where
  liftA2 f (x1,x2) (y1,y2) = (f x1 y1, f x2 y2)
instance Apply (TupleP 2) where
  (<*>) (f1,f2) (x1,x2) = (f1 x1, f2 x2)

type instance FunctorT (TupleP 3) a = (a,a,a)
instance Functor FunctionP (TupleP 3) where
  fmap f (x1,x2,x3) = (f x1, f x2, f x3)
instance Lift (TupleP 3) where
  liftA2 f (x1,x2,x3) (y1,y2,y3) = (f x1 y1, f x2 y2, f x3 y3)
instance Pure (TupleP 3) where
  pure x = (x,x,x)
instance Apply (TupleP 3) where
  (<*>) (f1,f2,f3) (x1,x2,x3) = (f1 x1, f2 x2, f3 x3)

type instance FunctorT (TupleP 4) a = (a,a,a,a)
instance Functor FunctionP (TupleP 4) where
  fmap f (x1,x2,x3,x4) = (f x1, f x2, f x3, f x4)
instance Pure (TupleP 4) where
  pure x = (x,x,x,x)
instance Lift (TupleP 4) where
  liftA2 f (x1,x2,x3,x4) (y1,y2,y3,y4) = (f x1 y1, f x2 y2, f x3 y3, f x4 y4)
instance Apply (TupleP 4) where
  (<*>) (f1,f2,f3,f4) (x1,x2,x3,x4) = (f1 x1, f2 x2, f3 x3, f4 x4)

type instance FunctorT (TupleP 5) a = (a,a,a,a,a)
instance Functor FunctionP (TupleP 5) where
  fmap f (x1,x2,x3,x4,x5) = (f x1, f x2, f x3, f x4, f x5)
instance Pure (TupleP 5) where
  pure x = (x,x,x,x,x)
instance Lift (TupleP 5) where
  liftA2 f (x1,x2,x3,x4,x5) (y1,y2,y3,y4,y5) = (f x1 y1, f x2 y2, f x3 y3, f x4 y4, f x5 y5)
instance Apply (TupleP 5) where
  (<*>) (f1,f2,f3,f4,f5) (x1,x2,x3,x4,x5) = (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5)

-- Non function category functors

instance Functor cat p => Functor (FunctorCategoryP (BasicFunctorP Maybe) cat) p where
  fmap = fmap fmap

instance Functor cat p => Functor (FunctorCategoryP (BasicFunctorP []) cat) p where
  fmap = fmap fmap

instance Functor cat p => Functor (Identity cat) p where
  fmap = fmap fmap
