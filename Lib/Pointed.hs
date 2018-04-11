-----------------------------------------------------------------------------
--
-- Module      :  testing Monoid deriving
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , DeriveAnyClass
    , DefaultSignatures
      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Pointed
    where

--import Data.Text as T
--import Data.Text.IO as T
import GHC.Generics

import Control.Monad.Error  -- is monads-tf

import Uniform.Zero

class Pointed f where
  point :: a -> f a
  default point :: (Generic1 f, Pointed (Rep1 f)) => a -> f a
  point = to1 . point

instance Pointed c => Pointed (M1 i t c) where
  point = M1 . point

instance Monoid c => Pointed (K1 i c) where
  point _ = K1 mempty

instance Pointed U1 where
  point _ = U1

instance Pointed r => Pointed (l :+: r) where
  point = R1 . point

instance (Pointed l, Pointed r) => Pointed (l :*: r) where
  point x = point x :*: point x

instance Pointed Par1 where
  point = Par1

instance Pointed f => Pointed (Rec1 f) where
  point = Rec1 . point

instance (Pointed l, Pointed r) => Pointed (l :.: r) where
  point = Comp1 . point . point

data Three a = Three a a a deriving (Show,Generic1, Pointed)

-- >>> point True :: Three Bool
--Three True True True

instance Pointed []

-- >>> point True :: [Bool]

