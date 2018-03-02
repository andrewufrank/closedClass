-----------------------------------------------------------------------------
--
-- Module      :  an example of the simplest, most direct solution for recognition of tags
-- and producing names for RDF
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
--, DataKinds, DeriveAnyClass, DefaultSignatures
    #-}
module ParseDepCode
    where


import           Test.Framework
import Uniform.Strings
--import  GHC.Generics
import qualified Data.Map as Map
import  Data.Map (Map (..))
--import Data.Utilities
import qualified Data.Text as T
import Data.Text (Text)
import Safe
--import Data.Maybe (Maybe (..))
--import           Data.Char
--import           Data.Map
--import           GHC.Generics    (Generic,Rep(..),(:+:)(..))
--import           GHC.TypeLits
--import Data.Proxy

--import Text.ParserCombinators.Parsec
--
----import           Protolude       hiding(fromLabel)
----import qualified Data.Text       as T
--import qualified GHC.Generics    as G

class (Ord a, Eq a, Read a, Show a) => POStags a where
-- , Generic a, Serialize a
    fromTag :: a -> Text
    parseTag :: Text -> a
    tagUNK :: a

class (Ord a, Eq a, Read a, Show a) => DEPtags a where
  fromDEPtag :: a -> Text
  parseDEPtag :: Text -> a
  tagDEPunk :: a
  notDEPtag :: a
--  default notChunkTag :: Bounded a => a
--  notChunkTag = maxBound

instance DEPtags DepCode where
  fromDEPtag (DepCode c1 c2) = if c2==Dep2Zero then showT c1
                                               else T.concat [showT c1, ":", showT c2]
  parseDEPtag   = readDepCode
  tagDEPunk  =  DepCode DepUnk Dep2Zero

data DepCode1 = ACL
                | ADVCL
                | ADVMOD
                | AMOD
                | APPOS
                | AUX
                | AUXPASS
                | CASE
                | CC  -- was CC but gives conflict with Conll.Tag
                | CCOMP
                | REPARANDUM
                | ROOT
                | VOCATIVE
                | XCOMP
                | DepUnk

                deriving (Show, Read, Eq, Ord, Enum, Bounded)
--instance CharChains2 DepCode1 Text where
--    show' = s2t . show

data DepCode2 = RELCL
            | AS  -- is this all prepositions?
            | ON
            | PREDET
            | TOWARDS
--            | MissingDepCode2 Text
            | Dep2Zero

    deriving (Show, Read, Eq, Ord, Enum, Bounded)


--instance CharChains2 DepCode2 Text where
--    show' = s2t . show
--

data DepCode = DepCode {d1::DepCode1
                        , d2 :: DepCode2
                        }
            | DepUnknown {s :: Text }
                deriving (Show, Read, Eq, Ord)
--instance CharChains2 DepCode Text where
instance Arbitrary DepCode1 where
  arbitrary = elements [minBound ..]
instance Arbitrary DepCode2 where
  arbitrary = elements [minBound ..]


--allTags :: [DEPtag]
--allTags = [minBound .. ]
--
--list1 = map showT allTags
--
map1 :: Map DepCode1 Text
map1 = Map.fromList $ zip [ACL .. ] (map showT [ACL ..])
--
map2 :: Map DepCode2 Text
map2 = Map.fromList $ zip [RELCL .. ] (map showT [RELCL ..])

readDepCode :: Text -> DepCode
readDepCode t = case length ts of
                    0 -> unk
                    1 -> maybe unk (\c  -> DepCode c  Dep2Zero) c1
                    2 -> case c2 of
                            Nothing -> unk
                            Just c22 -> case c1 of
                                Nothing -> unk
                                Just c11 -> DepCode c11 c22


    where
            ts = T.splitOn ":" t :: [Text]
            unk = DepUnknown t
            c1 =    (reverseLookup map1 . head $ ts) :: Maybe DepCode1
            c2 =    (reverseLookup map2 . head . tail $ ts):: Maybe DepCode2


test_1a = assertEqual (DepCode ACL Dep2Zero) (readDepCode "ACL")
test_2a = assertEqual (DepCode AUX ON) (readDepCode "AUX:ON")
test_2b = assertEqual (DepCode AUX Dep2Zero) (readDepCode "AUX")

test_3a = assertEqual "AUX:ON" (fromDEPtag $ DepCode AUX ON)
test_3b = assertEqual "AUX" (fromDEPtag $ DepCode AUX Dep2Zero)
--depCodeParser :: GenParser Char st DepCode
--depCodeParser
--test_1 = assertEqual ("AUX") (fromTag AUX)
--test_2 = assertEqual "XCOMP" (fromTag XCOMP)
--test_3 = assertEqual WP_Dollar (parseTag "WP$")
--test_4 = assertEqual CC (parseTag "CC")
--
--prop_tagsRoundTrip :: DEPtag -> Bool
--prop_tagsRoundTrip tag = tag == (parseTag . fromTag) tag
--
--
--instance DEPtags DEPtag where
--    fromTag a = maybe "UNKNOWN" id $ Map.lookup a map3
--    tagUNK = UNKNOWN
--    parseTag t = maybe tagUNK id $ Map.lookup t reverseLabelMap
--
--aOp = putStrLn "aOp executed"
--
--
reverseMap:: Map a Text -> Map  Text a
reverseMap m1 = Map.fromList [ (b,a) | (a,b) <- Map.assocs m1]

reverseLookup :: Map a Text -> Text -> Maybe a
reverseLookup m1 a1  = Map.lookup a1 (reverseMap m1)
