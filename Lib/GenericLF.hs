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

--import Data.Text as T
--import Data.Text.IO as T
import GHC.Generics

import Control.Monad.Error  -- is monads-tf

import Uniform.Zero

class ListForms1 l   where
--    type LF l
--    mkOne :: (LF l) -> l
--    default mkOne :: (ListForms1 l, LF1 (Rep x)) => x -> l x
--    mkOne = to gmkOne

    appendTwo  :: l    -> l   -> l
    default appendTwo :: (Generic (l), ListForms1 l, LF1 (Rep l)) => l   -> l    -> l
    appendTwo x y = to (gappendTwo (from x) (from y))


class LF1 l where
--    type LFG l
--    gmkOne :: (LFG l) -> l
    gappendTwo :: l x  -> l x  -> l x


instance LF1 U1 where   -- this is for zero
--  gmkOne x = U1
  gappendTwo U1 U1 = U1

instance  (ListForms1 a) => LF1 (K1 i a) where
--  gmkOne x = K1 (mkOne x)
  gappendTwo (K1 x) (K1 y) = K1 (x `appendTwo` y)

instance LF1 f => LF1 (M1 i c f) where
--  mkOne = M1 mkOne
  gappendTwo (M1 x) (M1 y) = M1 (x `gappendTwo` y)

instance (LF1 f, LF1 h) => LF1 (f :*: h) where
--  mkOne = mkOne :*: mkOne
  gappendTwo (x1 :*: y1) (x2 :*: y2) = gappendTwo x1 x2 :*: gappendTwo y1 y2

-----------------------
class Single l   where
--    type LFx l
    mkOne :: x -> l
    default mkOne :: (Generic l, Generic x, Single l, Gsingle (Rep l)) => x -> l
    mkOne x = to ( gmkOne (from x))

class Gsingle l where
--    type LFG l
    gmkOne :: x -> l x
instance Gsingle U1 where   -- this is for zero
  gmkOne x = U1

instance  (Gsingle a) => Gsingle (K1 i a) where
    gmkOne (K1 x) = K1 (mkOne x)

instance Single f => Gsingle (M1 i c f) where
  gmkOne (M1 x1) = M1 (gmkOne x1)

instance (Gsingle f, Gsingle h) => Gsingle (f :*: h) where
  gmkOne  (x1 :*: y1) = gmkOne x1:*: gmkOne y1

--------------------------------------------------------------------------------

--memptydefault :: (Generic a, ListForms1 (Rep a)) => a
--memptydefault = to mempty'
--
--mappenddefault :: (Generic a, ListForms1 (Rep a)) => a -> a -> a
--mappenddefault x y = to (appendTwo (from x) (from y))



