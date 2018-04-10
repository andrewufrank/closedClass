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

module GenericLF
    where

import Data.Text as T
import Data.Text.IO as T
import GHC.Generics

import Control.Monad.Error  -- is monads-tf

import Uniform.Zero

class ListForms1 l   where
    prependOne  :: x -> l x   -> l x
    appendOne :: l x  -> x -> l x

    mkOne :: x -> l x
--    default mkOne :: (ListForms1 l, LF1 (Rep x)) => x -> l x
--    mkOne = to gmkOne

--    unMakeOne :: l -> (LF l)  -- succeds only for singleton, not for export
    appendTwo  :: l x  -> l x  -> l x
    default appendTwo :: (Generic (l x), ListForms1 l, LF1 (Rep (l x))) => l x  -> l x  -> l x
    appendTwo x y = to (gappendTwo (from x) (from y))

    prependOne a la = appendTwo  (mkOne a) la
    appendOne la a = appendTwo la (mkOne a)
--    appendTwo = (<>)
    {-# Minimal appendTwo, mkOne #-}

class LF1 l where
    gmkOne :: x -> l x
    gappendTwo :: l x -> l x -> l x


instance LF1 U1 where   -- this is for zero
--  gmkOne x = U1
  gappendTwo U1 U1 = U1

instance  (ListForms1 a) => LF1 (K1 i (a x)) where
--  gmkOne x = K1 (mkOne x)
  gappendTwo (K1 x) (K1 y) = K1 (x `appendTwo` y)

instance ListForms1 f => LF1 (M1 i c f) where
--  mkOne = M1 mkOne
  gappendTwo (M1 x) (M1 y) = M1 (x `appendTwo` y)

instance (ListForms1 f, ListForms1 h) => LF1 (f :*: h) where
--  mkOne = mkOne :*: mkOne
  gappendTwo (x1 :*: y1) (x2 :*: y2) = appendTwo x1 x2 :*: appendTwo y1 y2

--------------------------------------------------------------------------------

--memptydefault :: (Generic a, ListForms1 (Rep a)) => a
--memptydefault = to mempty'
--
--mappenddefault :: (Generic a, ListForms1 (Rep a)) => a -> a -> a
--mappenddefault x y = to (appendTwo (from x) (from y))



