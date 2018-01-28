{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Data.Utilities

-- a collection of utilities used in construction of tags

where

import qualified Data.Text as T
import Data.Text (Text)
import qualified         Data.Char      as S          (isSpace, isLower, toLower, toUpper)
import Text.Read (readEither)
import           Test.Invariant
import Test.Framework
import Data.Text.Arbitrary

type ErrOrVal a = Either Text a
-- ^ a type for recording errors as text

-- | Just a handy alias for Text
type Error = Text


toEitherErr :: Either String a -> Either Error a
-- ^ convert a string error return to a text
-- better name toErrOrVal
toEitherErr (Left s) = Left (T.pack s)
toEitherErr (Right r) = Right r

readOrErr :: Read a => Text -> Either Text a
readOrErr    t = case (readEither (t2s t)) of
                        Left msg -> Left (s2t msg)
                        Right a -> Right a

s2t :: String -> Text
-- ^ String to Text (invertable)
s2t = T.pack

t2s :: Text -> String
-- ^ String to Text (invertable)
t2s = T.unpack

--toUpper' :: Text -> Text
--toUpper' = map T.toUpper
-- -- ^ convert all char to uppercase

toUpperStart :: Text -> Text
-- ^ convert the first character to Uppercase - for  PosTags in Spanish
toUpperStart t = (S.toUpper . T.head $ t ) `T.cons` (T.tail t)

replaceAll :: [(Text, Text)] -> Text -> Text
replaceAll patterns = foldl (.) id (map (uncurry  T.replace) (reversePatterns patterns))

reversePatterns :: [(Text, Text)] ->  [(Text, Text)]
reversePatterns = map (\(x,y) -> (y,x))

prop_inversePatterns :: [(Text, Text)] -> Bool
prop_inversePatterns = involutory  reversePatterns

normalized :: [(Text, Text)] ->  Text -> Text
normalized tagTxtPatterns = replaceAll tagTxtPatterns . T.toUpper

normalizedx :: [(Text, Text)] ->  Text -> Text
normalizedx tagTxtPatterns = replaceAll tagTxtPatterns

prop_inverse :: [(Text, Text)] ->  Text  -> Bool
prop_inverse tagTxtPatterns txt  =
    if  null tagTxtPatterns
         || (any (== ""). map fst $ tagTxtPatterns )
         || (any (== ""). map snd $ tagTxtPatterns )
         || txt==""
        then True
        else inverts (normalizedx tagTxtPatterns)
                                            (replaceAll tagTxtPatterns) ( txt)

test_normalize = assertEqual ("paula") (normalizedx txtpat "paule")
test_replaceAll = assertEqual ("peule") (replaceAll txtpat "paule")
test_replaceAll2 = assertEqual ("peule") (replaceAll txtpat "paula")
test_reverse = assertEqual txtpat (reversePatterns $ reversePatterns txtpat)

txtpat = [("a", "e")]
