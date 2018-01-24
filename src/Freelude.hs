{-|
Module      : Freelude
Description : Tutorial in this module
Copyright   : (c) Clinton Mead 2017
License     : BSD3
Maintainer  : clintonmead@gmail.com
Stability   : experimental
Portability : POSIX

= Overview

Freelude is a replacement prelude which generalises classes like Category and Functor,
that is, allows one to make more types Categories and Functors than before.

The structure and design is a bit of a work in progress.

This package also includes classes like Applicative and Monad, but they are not as generalised as Functor,
as I'm as yet unsure of a sensible way to generalise them.

I've tried to implement a lot of the ordinary instances in base and other GHC supplied modules,
but I'm probably missing some, so feel free to mention those which are missing or better still, just submit a patch.

This module itself simply reimports Prelude but overrides a bunch of functions and classes, and also adds a number of classes and functions.

Later on I'll probably develop a finer-grained approach.

= Motivation

Wouldn't it be nice if we could write something like this:

> (f1,f2) . (g1,g1)

And get what we expect, namely (f1 . g1, f2 . g2).

So lets try to write a 'Control.Category' instance for pairs:

@
instance Category ???
@

There's nothing sensible we can replace the ???s with.

This package resolves the above issue, and many others.

For the moment, this documentation will just show some examples of how these new versions of
'Category', 'Functor' etc can be used.

= Usage

Firstly, we want to ignore the normal "Prelude" and instead import "Freelude":

> {-# LANGUAGE NoImplicitPrelude #-}
> import Freelude

== Category

Here's how to run categorical compostion on pairs:

>>> :{
let
  f_pair :: (Read a, Enum b) => (String -> a, b -> b)
  f_pair = (read, succ)
  g_pair :: (Show b) => (Int -> Int, b -> String)
  g_pair = ((*3), show)
  applyPair (f1,f2) (x1,x2) = (f1 x1, f2 x2)
:}

>>> let h_pair = g_pair . f_pair
>>> applyPair h_pair ("5",'a')
(15,"'b'")

Note that we've successfully made @f@ and @g@, both tuples, into categories.

It's not just tuples we can turn into categories.

>>> :{
let
  f_list :: Num a => [a -> a]
  f_list = [(+2), (*3)]
  g_list :: Num a => [a -> a]
  g_list = [(+4), (*5)]
:}

>>> let h_list = f_list . g_list
>>> h_list <*> [6,7]
[12,13,32,37,30,33,90,105]

Also, here's a 'Maybe' category:

>>> ((Just (*2)) . Nothing) <*> (Just 4)
Nothing

>>> ((Just (*2)) . (Just (*3))) <*> (Just 4)
Just 24

It's worth noting that we have also extended the '$' operator to work for non ordinary functions:

>>> h_pair $ ("5",'a')
(15,"'b'")

>>> h_list $ [6,7]
[12,13,32,37,30,33,90,105]

>>> ((Just (*2)) . Nothing) $ (Just 4)
Nothing

>>> ((Just (*2)) . (Just (*3))) $ (Just 4)
Just 24

In cases where the category is also a functor (like 'Maybe', '[]'), '$' is generally just '<*>',
but '$' is somewhat more general (it's defined for 'Control.Arrow.Kleisli' arrows for example).

We can also nest categories:

>>> :{
let
  f_list_maybe :: Num a => [Maybe (a -> a)]
  f_list_maybe = [Just (+2), Just (*3)]
  g_list_maybe :: Num a => [Maybe (a -> a)]
  g_list_maybe = [Just (+4), Nothing]
:}

>>> let h_list_maybe = f_list_maybe . g_list_maybe
>>> h_list_maybe $ [Just 6,Nothing]
[Just 12,Nothing,Nothing,Nothing,Just 30,Nothing,Nothing,Nothing]

Above is also an example of how '$' is more general than '<*>'.

== Functor

Whilst we have 'Prelude.Functor's on pairs in the usual "Prelude", their definition is a little bit weird, namely the below:

@fmap f (x,y) = (x, f y)@

For example:

>>> Prelude.fmap (*3) (1,2)
(1,6)

This does have some uses but I consider it surprising. arguably a more sensible definition is as follows:

@fmap f (x,y) = (f x, f y)@

>>> :set -XTypeFamilies
>>> :set -XFlexibleContexts
>>> :set -XTypeInType
>>> fmap (*3) (1,2)
(3,6)

The above definition can not be defined with the normal instance of 'Prelude.Functor', but it be defined using "Freelude"s 'Functor'

We can also define functors over things like 'Data.Set', which we couldn't before.

>>> import qualified Data.Set as Set
>>> fmap (\x -> x * x) (Data.Set.fromList [1,-1,2,-2,3])
fromList [1,4,9]

And even unboxed arrays!

>>> import Data.Array.Unboxed (UArray)
>>> import Data.Array.IArray as IArray
>>> let a = (IArray.array (1,3) [(1,4),(2,5),(3,6)]) :: UArray Int Int
>>> fmap (*3) a
array (1,3) [(1,12),(2,15),(3,18)]

Note that both in the case of sets and unboxed arrays, ordinary 'Prelude.Functor' instances
can not be defined for them as there are constraints on their parameters.

We also further generalise 'Functor'. Instead of thinking of 'Functor'
as a function between functions, we think of it as a function between categories.
So `fmap` can be defined on other categories.

For example, we can define `fmap` on the category of a list of categories, like so:

>>> let f_l = (fmap [(+2),(*3),(+4)]) :: Num a => [Maybe a -> Maybe a]
>>> :t f_l
f_l :: Num a => [Maybe a -> Maybe a]

And indeed, this works roughly how we would expect:

>>> f_l <*> [Just 3, Nothing]
[Just 5,Nothing,Just 9,Nothing,Just 7,Nothing]

GHC even doesn't complain when we try to assert the function laws,
as long as we @AllowAmbiguousTypes@.

>>> :set -XAllowAmbiguousTypes
>>> :{
let
  eqT :: a -> a -> ()
  eqT _ _ = ()
  checkLaws x y = (fmap x . fmap y) `eqT` (fmap (x . y))
:}

== Applicatives

"Freelude" splits 'Prelude.Applicative' into three classes, namely:

1. 'Lift' ('liftA2')
2. 'Pure' ('pure')
3. 'Apply' ('<*>')

In the ordinary prelude, 'Prelude.<*>' is the primary function to be defined,
which has a signature as follows:

> f (a -> b) -> f a -> f b

But notice how the structure is required to be able to contain functions.
But we've already mentioned two strutures that can't: sets and unboxed arrays.

As a result, whereas in the "Prelude", 'Prelude.<*>' is the primary function and
'Control.Applicative.liftA2' is defined in terms of it, for "Freelude" we've switched that around.

In "Freelude", 'liftA2' is the primary definition
and '<*>' is optionally defined in terms of 'liftA2',
but only automatically when there is no restrictions on the type.

Note that unlike `Functor' I haven't generalised 'Lift', 'Pure' and 'Apply'
to non function categories. This perhaps could be done in the future but there's some
thinking to do about the best and most useful way forward with this.

Here's an example of 'liftA2' on sets:

>>> liftA2 (*) (Data.Set.fromList [1,2,3,4]) (Data.Set.fromList [1,2,3,4])
fromList [1,2,3,4,6,8,9,12,16]

And we have full applicative on tuples:
>>> (*) <$> (2,3) <*> (4,5)
(8,15)

== Monad

Monad is defined as a subclass of 'Lift' and 'Pure', instead of 'Apply' and 'Pure' (i.e. @Applicative@).
The reason for this is that interestingly, although you can't
define @<*>@ for @Set@ as discussed above, you can define @>>=@.

This is because the definition of @>>=@ is as follows:

> f a -> (a -> f b) -> f b

Note we don't have any of @f (a -> b)@ arguments that got us caught up trying to define @<*>@ for sets.

>>> (Data.Set.fromList [1,2,3]) >>= (\x -> Data.Set.fromList [x,x*x,x+2])
fromList [1,2,3,4,5,9]

== Rebindable syntax

You can use the extension @RebindableSyntax@ to use do-notation:

>>> :set -XRebindableSyntax
>>> :{
do
  set_x <- Data.Set.fromList [1,2,3]
  set_y <-(\x -> Data.Set.fromList [x,x*x,x+2]) set_x
  pure set_y
:}
fromList [1,2,3,4,5,9]

= Defining your own instances

I'll put a tutorial here at some point, but there's plenty of examples in "Freelude.Impl.Category"

= Rationale behind design

I'll fill this out at some point also, noting that it currently makes quite significant use of
injective type families and constraint kinds. Since the design is still in a state of flux
it's probably not detailling yet anyway (I welcome suggestions/patches).

It worth noting that some of the reasons for the design is to maintain the ability for the
type system to recognise certain invariants, which I will detail with examples in future.

= Notes

You'll probably want GHC 8.2. Some things will work with 8.0, indeed the library itself should compile,
but the tests won't.

In developing this library I've occasionally experienced GHC panics.
One thing that occasionally resolved this was including the extension @TypeInType@ in code that
uses the library. If you get a GHC panic perhaps try adding the extension @TypeInType@
and trying again.

A number of libraries that this depend on are build for and have dependencies on base for GHC 8.0,
This will cause dependency hell, but building this library with '--allow-newer' should fix all issues.

-}

module Freelude (
  module Prelude,
  module Freelude.Impl.Category,
  module Freelude.Impl.ExoFunctor
) where

import Freelude.Impl.Category
import Freelude.Impl.ExoFunctor
import Prelude hiding (
  Functor(fmap), (<$>), (<$),
  Applicative((<*>), pure), (<*), (*>),
  Monad(return, (>>=), (>>)), (=<<),
  (.), id, const,
  ($), ($!)
  )
