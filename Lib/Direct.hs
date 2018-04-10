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
module Direct
    where


import           Test.Framework
import Uniform.Strings
--import  GHC.Generics
import qualified Data.Map as Map
import  Data.Map (Map (..))
--import Data.Maybe (Maybe (..))
--import           Data.Char
--import           Data.Map
--import           GHC.Generics    (Generic,Rep(..),(:+:)(..))
--import           GHC.TypeLits
--import Data.Proxy
--
----import           Protolude       hiding(fromLabel)
----import qualified Data.Text       as T
--import qualified GHC.Generics    as G

class (Ord a, Eq a, Read a, Show a) => POStags a where
-- , Generic a, Serialize a
    fromTag :: a -> Text
    parseTag :: Text -> a
    tagUNK :: a

--    tagTerm :: a -> Text
--    startTag :: a
--    endTag :: a
--    -- | Check if a tag is a determiner tag.
--    isDt :: a -> Bool
--
--    -- lower level default implementations
--    showTag2 :: [(Text, Text)] -> a -> Text
--    showTag2 tagTxtPatterns tag = replaceAll (reversePatterns tagTxtPatterns) (s2t $ show tag)
--
--    readTag2 :: [(Text, Text)] -> Text -> a
--    readTag2 tagTxtPatterns = either (return tagUNK) id . readOrErr . normalized tagTxtPatterns

--    normalized :: [(Text, Text)] ->  Text -> a
--    normalized tagTxtPatterns = replaceAll tagTxtPatterns (T.toUpper txt)


-- reduced !
data POStag = CC
         | CD
         | ClosePar
         | Colon
         | Coma
         | Dash
         | Dollar
         | DT
         | EX
         | LRB_
         | LS
         | MD
         | PRP_Dollar
         | Quotes
         | Quotes2
         | RB

         | WP
         | WP_Dollar
         | WRB
         | UNKNOWN -- when some unknown string used
         deriving(Show,Read,Eq,Ord, Enum, Bounded)  -- ,Generic,TagLabel)

spelledAs =
         [  (ClosePar, ")"  )
         , (Colon,   ":"    )
         , ( Coma, ",")
         , ( Dash,  "--"   )
         , (Dollar , "$"    )

         , (LRB_,  "-LRB-")
        , (PRP_Dollar, "PRP$")
         , (Quotes , "''")
         , (Quotes2 , "``")
         , (WP_Dollar, "WP$")
         ] :: [(POStag,Text)]

instance Arbitrary POStag where
  arbitrary = elements [minBound ..]


allTags :: [POStag]
allTags = [CC .. UNKNOWN]

list1 = map showT allTags

map1, map2, map3 :: Map POStag Text
map1 = Map.fromList $ zip allTags list1

map2 = Map.fromList spelledAs
-- show produces the "xx"

map3 = Map.union map2 map1
-- map2 values override map1 values
test_1 = assertEqual ("CC") (fromTag CC)
test_2 = assertEqual "WP$" (fromTag WP_Dollar)
test_3 = assertEqual WP_Dollar (parseTag "WP$")
test_4 = assertEqual CC (parseTag "CC")

prop_tagsRoundTrip :: POStag -> Bool
prop_tagsRoundTrip tag = tag == (parseTag . fromTag) tag


instance POStags POStag where
    fromTag a = maybe "UNKNOWN" id $ Map.lookup a map3
    tagUNK = UNKNOWN
    parseTag t = maybe tagUNK id $ Map.lookup t reverseLabelMap

aOp = putStrLn "aOp executed"


reverseLabelMap :: Map  Text POStag
reverseLabelMap = Map.fromList [ (b,a) | (a,b) <- Map.assocs map3]
