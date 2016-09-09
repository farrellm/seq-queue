{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Sequence.Queue
  (Queue(..)
  ,(|>)
  ,(><)
  ,ViewQ(..)
  ,deq
  ) where

import Prelude
       hiding (null, length, replicate, filter, splitAt, take, drop,
               break, span, dropWhile, takeWhile)
import           Control.Applicative (Alternative)
import           Control.Monad (MonadPlus)
import qualified Data.Sequence as S
-- import           GHC.Exts
import           Data.String
import           Data.Data
import           Data.MonoTraversable
import           Data.Sequences

newtype Queue a =
  Queue {getQueue :: S.Seq a}
  deriving (Read,Show,Data,Eq,Ord,Monoid,
            Foldable,Traversable,
            Functor,Applicative,Monad,Alternative,MonadPlus)
-- IsList,NFData,IsString
-- deriving instance IsString (Queue (S.Seq Char))

-- instance GHC.Exts.IsList(Queue (S.Seq a)) where
--   type Item (Queue (S.Seq a)) = a
--   fromList l = Queue (fromList l)

type instance Element (Queue a) = a

deriving instance MonoFoldable (Queue a)
deriving instance MonoFunctor (Queue a)
instance MonoTraversable (Queue a)

-- Applicative
instance MonoPointed (Queue a)

instance GrowingAppend (Queue a)

instance SemiSequence (Queue a) where
    type Index (Queue a) = Int
    cons a (Queue q) = Queue (a S.<| q)
    {-# INLINE cons #-}
    snoc (Queue q) a = Queue (q S.|> a)
    {-# INLINE snoc #-}
    reverse (Queue q) = Queue (S.reverse q)
    {-# INLINE reverse #-}
    sortBy f (Queue q) = Queue (S.sortBy f q)
    {-# INLINE sortBy #-}

    intersperse a (Queue q) = Queue (intersperse a q)
    {-# INLINE intersperse #-}
    find a (Queue q) = find a q
    {-# INLINE find #-}

queuePair :: (S.Seq a, S.Seq b) -> (Queue a, Queue b)
queuePair (a, b) = (Queue a, Queue b)
{-# INLINE queuePair #-}

queueWrap :: (S.Seq a -> S.Seq b) -> Queue a -> Queue b
queueWrap f (Queue q) = Queue (f q)
{-# INLINE queueWrap #-}

queueWrapPair :: (S.Seq a -> (S.Seq b, S.Seq c))
              -> Queue a
              -> (Queue b, Queue c)
queueWrapPair f (Queue q) = queuePair (f q)
{-# INLINE queueWrapPair #-}

instance IsSequence (Queue a) where
    fromList = Queue . fromList
    replicate n a = Queue (replicate n a)
    replicateM n a = Queue <$> replicateM n a
    filter p = queueWrap (filter p)
    --filterM = filterM
    break p = queueWrapPair (break p)
    span p = queueWrapPair (span p)
    dropWhile p = queueWrap (dropWhile p)
    takeWhile p = queueWrap (takeWhile p)
    splitAt n = queueWrapPair (splitAt n)
    take n = queueWrap (take n)
    drop n = queueWrap (drop n)
    partition p = queueWrapPair (partition p)
    uncons (Queue q) = (\(x, xs) -> (x, Queue xs)) <$> uncons q
    unsnoc (Queue q) = (\(xs, x) -> (Queue xs, x)) <$> unsnoc q
    --groupBy = Seq.groupBy
    tailEx = queueWrap tailEx
    initEx = queueWrap initEx
    {-# INLINE fromList #-}
    {-# INLINE break #-}
    {-# INLINE span #-}
    {-# INLINE dropWhile #-}
    {-# INLINE takeWhile #-}
    {-# INLINE splitAt #-}
    {-# INLINE take #-}
    {-# INLINE drop #-}
    {-# INLINE partition #-}
    {-# INLINE uncons #-}
    {-# INLINE unsnoc #-}
    {-# INLINE filter #-}
    {-# INLINE replicate #-}
    {-# INLINE replicateM #-}
    {-# INLINE tailEx #-}
    {-# INLINE initEx #-}
    index (Queue q) n = index q n
    indexEx (Queue q) n = indexEx q n
    unsafeIndex (Queue q) n = unsafeIndex q n
    {-# INLINE index #-}
    {-# INLINE indexEx #-}
    {-# INLINE unsafeIndex #-}


(|>) :: Queue a -> a -> Queue a
(Queue q) |> v = Queue (q S.|> v)
{-# INLINE (|>) #-}

(><) :: Queue a -> Queue a -> Queue a
(Queue p) >< (Queue q) = Queue (p S.>< q)
{-# INLINE (><) #-}

data ViewQ a = EmptyQ | a :< (Queue a)

deq :: Queue a -> ViewQ a
deq (Queue q) = case S.viewl q of
                      v S.:< q' -> v :< Queue q'
                      S.EmptyL -> EmptyQ
{-# INLINE deq #-}
