{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Freelude.FunctionRestrictedFunctor (fmap) where

import Prelude hiding (fmap)
import Freelude.Impl.RestrictedFunctor
import Freelude.Impl.Classes hiding (fmap)

fmap ::
  (Freelude.Impl.Classes.Functor FunctionP p, ra ~ FunctorT p a, rb ~ FunctorT p b, CategoryC FunctionP a b, CategoryC FunctionP ra rb, FunctorSrcC p a, FunctorDstC p b) =>
  CategoryT FunctionP a b -> CategoryT FunctionP ra rb
fmap = restrictedfmap
