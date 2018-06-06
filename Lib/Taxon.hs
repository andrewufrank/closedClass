-----------------------------------------------------------------------------
--
-- Module      :  the distinction which define a taxon
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

module Taxon (Taxon (..), Taxons(..)
    , Lattice (..), LatticeTests (..)
    , DistValue (..), dv2pair, pair2dv
    , B4val (..)
    , PartialRel (..)
    , htf_Taxon_thisModulesTests
    )  where

import Test.Framework
import Uniform.Strings
import Uniform.Error

import Distinction
import qualified Data.Map as Map
import Data.List

newtype Taxon d = Taxon (Map.Map d B4val)  deriving (Show, Read, Eq)
-- | taxon over a set of distinctions d

class (Show d) => Taxons t d where
    specialize :: d -> B4val -> t d -> t d
    specializeList :: [(d, B4val)] -> t d -> t d
    showTaxon :: t d -> Text
    unTaxon :: t d -> (Map.Map d B4val)

    dvList2taxon :: [DistValue d] -> t d
    taxon2dvList :: t d -> [DistValue d]

instance  (Show d, Eq d, Ord d) => Taxons Taxon d where
    specialize d v t = Taxon  . Map.insert d v . unTaxon $ t
    showTaxon t =   unwords' . map showDV
                                . taxon2dvList  $ t
    unTaxon (Taxon m) = m

    specializeList l t1= foldl (\t (d,v) -> specialize d v t) t1  l

--    dvList2taxon :: [DistValue d] -> Taxon d
    dvList2taxon   =  Taxon  . Map.fromList .   (map  dv2pair)
    taxon2dvList   =        map pair2dv . Map.toAscList . unTaxon

instance (Eq d, Bounded d, Enum d, Show d, Ord d) => Lattice (Taxon d) where
    lcompare = compareTaxon
    lmeet = meetTaxon
    ljoin = joinTaxon
    top =  dvList2taxon (makeAll None4)-- []
    bottom = dvList2taxon (makeAll Both4)
    isBottom = any (isBottom . v) . taxon2dvList
    isTop = all (isTop . v) . taxon2dvList

makeAll :: (Enum d, Bounded d) => B4val -> [DistValue d]
makeAll v = map (\d -> DV d v) allDist -- :: [DistValue d]
    where
            allDist = [minBound .. maxBound] -- :: [DistPaper]

joinTaxon :: (Eq d, Bounded d, Show d, Ord d) => Taxon d -> Taxon d -> Taxon d
joinTaxon a b = dvList2taxon l
    where
        aa = taxon2dvList a -- :: [DistValue d]
        bb = taxon2dvList b -- :: [DistValue d]
        l = zipWith ljoin aa bb -- :: [DistValue d]

meetTaxon :: (Eq d, Bounded d, Show d, Ord d) => Taxon d -> Taxon d -> Taxon d
meetTaxon a b = dvList2taxon l
    where
        aa = taxon2dvList a -- :: [DistValue d]
        bb = taxon2dvList b -- :: [DistValue d]
        l = zipWith lmeet aa bb -- :: [DistValue d]

compareTaxon :: (Eq d, Bounded d, Show d, Ord d) => Taxon d -> Taxon d -> PartialRel
compareTaxon a b = if a==b then PEQ
                    else if a==l then PGT
                    else if b==l then PLT
                    else INC
    where l = joinTaxon a b



instance (Eq d, Enum d, Bounded d, Show d, Ord d) => LatticeTests (Taxon d)


instance (Eq d, Enum d, Bounded d, Show d, Ord d, Arbitrary (DistValue d))
    => Arbitrary (Taxon d) where
    arbitrary = do
        d :: [DistValue d] <- arbitrary
        let d2 = makeAll None4 ++ d
        return $ dvList2taxon d2




