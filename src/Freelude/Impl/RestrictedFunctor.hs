{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Freelude.Impl.RestrictedFunctor (
  restrictedfmap
  ) where

import Prelude hiding (Functor(fmap), (<$>))
import Freelude.Impl.Category hiding (Functor(fmap), (<$>))
import qualified Freelude.Impl.Category

restrictedfmap ::
  (Freelude.Impl.Category.Functor FunctionP p, ra ~ FunctorT p a, rb ~ FunctorT p b, CategoryC FunctionP a b, CategoryC FunctionP ra rb, FunctorSrcC p a, FunctorDstC p b) =>
  CategoryT FunctionP a b -> CategoryT FunctionP ra rb
restrictedfmap = Freelude.Impl.Category.fmap
