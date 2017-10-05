{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Freelude.Impl.CategoryAsMonoid where

import Prelude hiding ((.), id)
import Freelude.Impl.Category
import Data.Semigroup (Semigroup((<>)))

newtype CategoryAsMonoid a = CategoryAsMonoid { getCategoryAsMonoid :: a }

instance (IsSemigroupoid t p a b, a ~ b) => Semigroup (CategoryAsMonoid t) where
  CategoryAsMonoid x <> CategoryAsMonoid y = CategoryAsMonoid (x . y)

instance (IsCategory t p a b, a ~ b) => Monoid (CategoryAsMonoid t) where
  mempty = CategoryAsMonoid id
  mappend = (<>)
