{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Freelude where

import Prelude hiding ((.))
import GHC.Exts (Constraint)
import qualified Data.Semigroup as BaseSemigroup
import Freelude.Impl.Semigroupoid (SemigroupoidT)

newtype BaseSemigroup t a = BaseSemigroup { fromBaseSemigroup :: SemigroupoidT t a a }

instance (Semigroup t) => BaseSemigroup.Semigroup (BaseSemigroup t a) where
  (BaseSemigroup x) <> (BaseSemigroup y) = BaseSemigroup (x <> y)


instance {-# OVERLAPPABLE #-} Semigroupoid t => Semigroup t where
  x <> y = x . y
