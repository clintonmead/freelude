{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeInType #-}

{-|
This module isn't really useful yet, see the following [issue](https://github.com/clintonmead/freelude/issues/1)
-}
module Freelude.Impl.Compat (
  ) where


import Prelude hiding (Functor, fmap, (<$>))

import qualified Freelude.Impl.Classes as Class
import Freelude.Impl.Classes (
  FunctionP,
  BasicFunctorP
--  FunctorT,
--  fmap,
--  FunctorSrcC, FunctorDstC
  )

class    Class.Functor FunctionP (BasicFunctorP f) => Functor f
instance Class.Functor FunctionP (BasicFunctorP f) => Functor f

--class    Class.Functor FunctionP (SimpleFunctorP src_c dst_c f) => UnrestrictedFunctor f
--instance Class.Functor FunctionP (SimpleFunctorP src_c dst_c f) => UnrestrictedFunctor f

--UnrestrictedFunctor f, Ord a => (a -> b) -> f a -> f b

--type MonoFunctor a t = forall p. (Functor p, FunctorT p a ~ t) => t
