{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}

module Freelude.Impl.ToKind (ToType, ToKind) where

import Data.Kind (Type)

type ToType a = ToKind Type a
type ToKind toK (a :: k) = ToKind' toK k a

type family ToKind' toK k (a :: k) where
  ToKind' k k a = a
