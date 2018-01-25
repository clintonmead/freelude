{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Freelude.Impl.RestrictedFunctor (
  restrictedfmap
  ) where

import Prelude hiding (Functor(fmap), (<$>))
import Freelude.Impl.Classes hiding (Functor(fmap), (<$>))
import qualified Freelude.Impl.Classes

restrictedfmap ::
  (Freelude.Impl.Classes.Functor FunctionP p, ra ~ FunctorT p a, rb ~ FunctorT p b, CategoryC FunctionP a b, CategoryC FunctionP ra rb, FunctorSrcC p a, FunctorDstC p b) =>
  CategoryT FunctionP a b -> CategoryT FunctionP ra rb
restrictedfmap = Freelude.Impl.Classes.fmap
