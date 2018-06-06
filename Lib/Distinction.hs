-----------------------------------------------------------------------------
--
-- Module      :  distinction values
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Distinction (Lattice (..), LatticeTests(..)
--    , Distinction, DistPaper (..)
    , DistValue (..), dv2pair, pair2dv, showDV
    , B4val (..), PartialRel(..)
--    , physObj'
--    , human', stuff'
    , htf_Distinction_thisModulesTests
    )
    where

import Test.Framework
import Uniform.Strings
import Uniform.Error
--import qualified Data.PartialOrd as PO
--import GHC.Generic

import Belnap


--instance (Bounded d, Enum d) => Arbitrary d where
--    arbitrary = arbitraryBoundedEnum

data DistValue dist = DV {d :: dist, v:: B4val} -- | DVtop | DVbot
            deriving (Show, Read, Eq, Ord)
            -- only True4 and Fals4 should occur, rest is always mapped to DVtop or DVbot?

----instance Show DistValue where
showDV :: Show d => DistValue d -> Text
showDV (DV d v) = s2t $  (show d) ++ s
        where s = case v of
                    True4 -> "+"
                    False4 -> "-"
                    Both4 -> "X"
                    None4 -> "*"




dv2pair (DV d v) = (d,v)
pair2dv (d,v) = DV d v


instance (Eq d, Bounded d) => Lattice (DistValue d) where
    lcompare2 a@(DV d1 v1) b@(DV d2 v2) = if d1 == d2 then lcompare v1 v2 else INC
    lmeet2  (DV d1 v1)  (DV d2 v2) = if d1 == d2 then (DV d1 (lmeet v1 v2)) else bottom
    ljoin2  (DV d1 v1)  (DV d2 v2) = if d1 == d2 then (DV d1 (ljoin v1 v2)) else top
    top = pair2dv (minBound, None4)  -- alternative DVtop
    bottom = pair2dv (minBound, Both4)   -- DVbot

--normalize a@(DV d v) = a -- if v==bottom then bottom else if v==top then top else a

instance (Eq d, Bounded d) =>  LatticeTests (DistValue d)





