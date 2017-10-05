{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Freelude.Impl.MonoidAsCategory (
  MonoidAsCategory(MonoidAsCategory, getMonoidAsCategory)
  ) where

import Freelude.Impl.Category
import Data.Void (Void)
import Data.Semigroup (Semigroup((<>)))
import Data.Monoid (Monoid(mempty))
import Data.Type.Equality (type (~~))

newtype MonoidAsCategory a = MonoidAsCategory { getMonoidAsCategory :: a }

type instance CategoryT (MonoidAsCategory a) Void Void = MonoidAsCategory a
type instance CategorySrcC (MonoidAsCategory _) a = (a ~~ Void)
type instance CategoryDstC (MonoidAsCategory _) b = (b ~~ Void)

instance Semigroup m => Semigroupoid (MonoidAsCategory m) where
  (MonoidAsCategory x) . (MonoidAsCategory y) = MonoidAsCategory (x <> y)

instance (Semigroup m, Monoid m) => Category (MonoidAsCategory m) where
  id = MonoidAsCategory mempty
