{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Freelude.Impl.CategoryAsMonoid (
  CategoryAsMonoid(getCategoryAsMonoid)
) where

import Prelude hiding ((.), id)
import Freelude.Impl.Classes
import Data.Semigroup (Semigroup((<>)))

newtype CategoryAsMonoid a = CategoryAsMonoid { getCategoryAsMonoid :: a }

instance (IsSemigroupoid t p a a) => Semigroup (CategoryAsMonoid t) where
  CategoryAsMonoid x <> CategoryAsMonoid y = CategoryAsMonoid (x . y)

instance (IsCategory t p a a, ExoIsCategory t p a a) => Monoid (CategoryAsMonoid t) where
  mempty = CategoryAsMonoid id
  mappend = (<>)
