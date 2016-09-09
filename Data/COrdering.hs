-----------------------------------------------------------------------------
-- |
-- Module      :  Data.COrdering
-- Copyright   :  (c) Adrian Hey 2004-2008
-- License     :  BSD3
--
-- Maintainer  :  http://homepages.nildram.co.uk/~ahey/em.png
-- Stability   :  stable
-- Portability :  portable
--
-- This module defines a useful variant of the "Prelude" `Ordering` data type.
--
-- Typically this data type is used as the result of a \"combining comparison\"
-- which combines values that are deemed to be equal (somehow). Note that the
-- functions defined here adhere to the same ordering convention as the overloaded
-- 'compare' (from the 'Ord' class). That is..
--
-- @
-- a \`compare\` b -> LT (or Lt) implies a < b
-- a \`compare\` b -> GT (or Gt) implies a > b
-- @
--
-- The combinators exported from this module have a \"CC\" suffix if they
-- return a combining comparison (most of them) and a \"C\" suffix if they return
-- an ordinary comparison. All the combinators defined here are INLINEd, in the hope
-- that the compiler can avoid the overhead of using HOFs for frequently
-- used comparisons (dunno if this does any good though :-)
-----------------------------------------------------------------------------
module Data.COrdering
        ( -- * Types
         COrdering(..),

         -- * Useful combinators

         -- ** Misc.
         unitCC,unitByCC,
         fstCC,fstByCC,
         sndCC,sndByCC,
         flipC,flipCC,

         -- ** For combining \"equal\" values with a user supplied function.
         withCC,withCC',withByCC,withByCC',

        ) where

import Data.Typeable

-- | Result of a combining comparison.
data COrdering a = Lt | Eq a | Gt deriving (Eq,Ord,Read,Show,Typeable)

#ifndef __GLASGOW_HASKELL__
-- A Typeable instance (not needed by ghc, but Haddock fails to document this instance)
instance Typeable e => Typeable (COrdering e) where
 typeOf = typeOfDefault
#endif

-- | A combining comparison for an instance of 'Ord' which returns unit () where appropriate.
{-# INLINE unitCC #-}
unitCC :: Ord a => (a -> a -> COrdering ())
unitCC a b = case compare a b of LT -> Lt
                                 EQ -> Eq ()
                                 GT -> Gt

-- | Create a combining comparison from an ordinary comparison by returning unit () where appropriate.
{-# INLINE unitByCC #-}
unitByCC :: (a -> b -> Ordering) -> (a -> b -> COrdering ())
unitByCC cmp a b = case cmp a b of LT -> Lt
                                   EQ -> Eq ()
                                   GT -> Gt

-- | A combining comparison for an instance of 'Ord' which keeps the first argument
-- if they are deemed equal. The second argument is discarded in this case.
{-# INLINE fstCC #-}
fstCC :: Ord a => (a -> a -> COrdering a)
fstCC a a' = case compare a a' of LT -> Lt
                                  EQ -> Eq a
                                  GT -> Gt

-- | Create a combining comparison from an ordinary comparison by keeping the first argument
-- if they are deemed equal. The second argument is discarded in this case.
{-# INLINE fstByCC #-}
fstByCC :: (a -> b -> Ordering) -> (a -> b -> COrdering a)
fstByCC cmp a b = case cmp a b of LT -> Lt
                                  EQ -> Eq a
                                  GT -> Gt

-- | A combining comparison for an instance of 'Ord' which keeps the second argument
-- if they are deemed equal. The first argument is discarded in this case.
{-# INLINE sndCC #-}
sndCC :: Ord a => (a -> a -> COrdering a)
sndCC a a' = case compare a a' of LT -> Lt
                                  EQ -> Eq a'
                                  GT -> Gt

-- | Create a combining comparison from an ordinary comparison by keeping the second argument
-- if they are deemed equal. The first argument is discarded in this case.
{-# INLINE sndByCC #-}
sndByCC :: (a -> b -> Ordering) -> (a -> b -> COrdering b)
sndByCC cmp a b = case cmp a b of LT -> Lt
                                  EQ -> Eq b
                                  GT -> Gt

-- | Create a combining comparison using the supplied combining function, which is applied if
-- 'compare' returns 'EQ'. See 'withCC'' for a stricter version of this function.
{-# INLINE withCC #-}
withCC :: Ord a => (a -> a -> b) -> (a -> a -> COrdering b)
withCC f a a' = case compare a a' of LT -> Lt
                                     EQ -> Eq (f a a')
                                     GT -> Gt

-- | Same as 'withCC', except the combining function is applied strictly.
{-# INLINE withCC' #-}
withCC' :: Ord a => (a -> a -> b) -> (a -> a -> COrdering b)
withCC' f a a' = case compare a a' of LT -> Lt
                                      EQ -> let b = f a a' in b `seq` Eq b
                                      GT -> Gt

-- | Create a combining comparison using the supplied comparison and combining function,
-- which is applied if the comparison returns 'EQ'. See 'withByCC'' for a stricter version
-- of this function.
{-# INLINE withByCC #-}
withByCC :: (a -> b -> Ordering) -> (a -> b -> c) -> (a -> b -> COrdering c)
withByCC cmp f a b = case cmp a b of LT -> Lt
                                     EQ -> Eq (f a b)
                                     GT -> Gt

-- | Same as 'withByCC', except the combining function is applied strictly.
{-# INLINE withByCC' #-}
withByCC' :: (a -> b -> Ordering) -> (a -> b -> c) -> (a -> b -> COrdering c)
withByCC' cmp f a b = case cmp a b of LT -> Lt
                                      EQ -> let c = f a b in c `seq` Eq c
                                      GT -> Gt

-- | Converts a comparison to one which takes arguments in flipped order, but
-- preserves the ordering that would be given by the \"unflipped\" version (disregarding type issues).
-- So it's not the same as using the prelude 'flip' (which would reverse the ordering too).
{-# INLINE flipC #-}
flipC :: (a -> b -> Ordering) -> (b -> a -> Ordering)
flipC cmp b a = case cmp a b of LT -> GT
                                EQ -> EQ
                                GT -> LT

-- | Converts a combining comparison to one which takes arguments in flipped order, but
-- preserves the ordering that would be given by the \"unflipped\" version (disregarding type issues).
-- So it's not the same as using the prelude 'flip' (which would reverse the ordering too).
{-# INLINE flipCC #-}
flipCC :: (a -> b -> COrdering c) -> (b -> a -> COrdering c)
flipCC cmp b a = case cmp a b of Lt       -> Gt
                                 e@(Eq _) -> e
                                 Gt       -> Lt


