{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Sequence.Queue
  (Queue(..)
  ,empty
  ,singleton
  ,(|>)
  ,(><)
  ,null
  ,length
  ,ViewQ(..)
  ,deq
  ) where

import           Prelude hiding (null, length)
import           Control.Applicative (Alternative)
import           Control.Monad (MonadPlus)
import qualified Data.Sequence as S
import           GHC.Exts
import           Data.String
import           Data.Data

newtype Queue a =
  Queue {getQueue :: S.Seq a}
  deriving (Read,Show,Data,Eq,Ord,Monoid,
            Functor,Applicative,Monad,Alternative,MonadPlus)
-- IsList,NFData,IsString
-- deriving instance IsString (Queue (S.Seq Char))

-- instance GHC.Exts.IsList(Queue (S.Seq a)) where
--   type Item (Queue (S.Seq a)) = a
--   fromList l = Queue (fromList l)

empty :: Queue a
empty = Queue S.empty
{-# INLINE empty #-}

singleton :: a -> Queue a
singleton v = Queue (S.singleton v)
{-# INLINE singleton #-}

(|>) :: Queue a -> a -> Queue a
(Queue q) |> v = Queue (q S.|> v)
{-# INLINE (|>) #-}

(><) :: Queue a -> Queue a -> Queue a
(Queue p) >< (Queue q) = Queue (p S.>< q)
{-# INLINE (><) #-}

null :: Queue a -> Bool
null (Queue q) = S.null q
{-# INLINE null #-}

length :: Queue a -> Int
length (Queue q) = S.length q
{-# INLINE length #-}

data ViewQ a = EmptyQ | a :< (Queue a)

deq :: Queue a -> ViewQ a
deq (Queue q) = case S.viewl q of
                      v S.:< q' -> v :< Queue q'
                      S.EmptyL -> EmptyQ
{-# INLINE deq #-}
