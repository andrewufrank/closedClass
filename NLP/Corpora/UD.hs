{-----------------------------------------------------------------------------
--
-- Module       | --  Dependency and other Codes
--
-- | the codes for UD  -- the table is lifted
--
-----------------------------------------------------------------------------}
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE        MultiParamTypeClasses
       , ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
        , TypeSynonymInstances
        , FlexibleInstances
        , DeriveAnyClass
        , DeriveGeneric
        #-}

module NLP.Corpora.UD (module NLP.Corpora.UD
        , NLP.POStags(..)
        )
         where

import Data.Serialize (Serialize)
import qualified Data.Text as T
import Data.Text (Text)
--import Data.Utilities
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

import GHC.Generics

import qualified NLP.Types.Tags as NLP
import NLP.Types.General
--import NLP.Types.Tree hiding (Chunk)
-- import NLP.Types.IOB

data POStag =   -- copied from http://universaldependencies.org/u/pos/
    START  | -- START tag, used in training.
    END | --END tag, used in training.
    ADJ | -- adjective
    ADP | -- adposition
    ADV | -- adverb
    AUX | -- auxiliary
    CCONJ | -- coordinating conjunction
    DET | -- determiner
    INTJ | -- interjection
    NOUN | -- noun
    NUM | -- numeral
    PART | -- particle
    PRON | -- pronoun
    PROPN | -- proper noun
    PUNCT | -- punctuation
    SCONJ | -- subordinating conjunction
    SYM | -- symbol
    VERB | -- verb
    X  -- other
        deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)

instance NLP.TagsetIDs POStag where
    tagsetURL _ = "http://universaldependencies.org/u/pos/"

instance NLP.POStags  POStag where
--parseTag :: Text -> PosTag
    parseTag txt = case readTag txt of
                   Left  _ -> NLP.tagUNK
                   Right t -> t

    tagUNK = X

    tagTerm = showTag

    startTag = START
    endTag = END

    isDt tag = tag `elem` [DET]

instance Arbitrary POStag where
  arbitrary = elements [minBound ..]
instance Serialize POStag

readTag :: Text -> ErrOrVal POStag
--readTag "#" = Right Hash
--readTag "$" = Right Dollar
--readTag "(" = Right Op_Paren
--readTag ")" = Right Cl_Paren
--readTag "''" = Right CloseDQuote
--readTag "``" = Right OpenDQuote
--readTag "," = Right Comma
--readTag "." = Right Term
--readTag ":" = Right Colon
readTag txt =
  let normalized = replaceAll tagTxtPatterns (T.toUpper txt)
  in  (readOrErr  normalized)

-- | Order matters here: The patterns are replaced in reverse order
-- when generating tags, and in top-to-bottom when generating tags.
tagTxtPatterns :: [(Text, Text)]
tagTxtPatterns = [ ("$", "dollar")
                 ]

reversePatterns :: [(Text, Text)]
reversePatterns = map (\(x,y) -> (y,x)) tagTxtPatterns

showTag :: POStag -> Text
--showTag Hash = "#"
--showTag Op_Paren = "("
--showTag Cl_Paren = ")"
--showTag CloseDQuote = "''"
--showTag OpenDQuote = "``"
--showTag Dollar = "$"
--showTag Comma = ","
--showTag Term = "."
--showTag Colon = ":"
showTag tag = replaceAll reversePatterns (s2t $ show tag)

replaceAll :: [(Text, Text)] -> (Text -> Text)
replaceAll patterns = foldl (.) id (map (uncurry  T.replace) patterns)

--readTag :: Text -> ErrOrVal POStag
--readTag txt = maybe2errorP . read . t2s $ txt
--
--maybe2errorP  :: Maybe a -> ErrOrVal a
--maybe2errorP Nothing = Left "readTag POStag 34232"
--maybe2errorP (Just a) = Right a

-- @since 4.6.0.0
--readOrErr :: Read a => Text -> Either Text a
--readOrErr    t = case (readEither (t2s t)) of
--                        Left msg -> Left (s2t msg)
--                        Right a -> Right a

--instance CharChains2 POStag String where
--    show' =  show
--instance CharChains2 POStag Text where
--    show' =  s2t . show
--
--instance Zeros POStag where zero = X
----type Unk = Conll.Unk



