-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards     #-}

module FirstSub
    where

import           Test.Framework
import Uniform.Strings
import Uniform.Error
import Uniform.FileIO
import Conllu.IO
import Conllu.Print as Pr
import Conllu.Type as T
import qualified Conllu.Parse as P -- (document, Parser)  -- this is the parser!
import qualified Text.Megaparsec as M
import CoreNLP.DocNLP_0or1
import CoreNLP.Doc1_absoluteID
import CoreNLP.Doc2ToLinear
import CoreNLP.Linear2Triple

import Data.RDFext.Codes
import qualified NLP.Corpora.UD as UD
import qualified NLP.Types.Tags      as NLP
import Text.Show.Pretty

firstMain :: ErrIO ()
firstMain = do
    putIOwords ["read a udfeat file and parse"]
    in1 <- readFile2 (makeRelFile "short1ud.txt")
    putIOwords ["read short1ud.txt", in1]
    res1 <- callIO $  readConllu "veryshort1ud.txt"
    putIOwords ["result from readConllu", showT res1]

    return ()

-- utilities
parseConllu :: P.Parser [T.Sentence] -> Text -> ErrOrVal [T.Sentence]
-- | parse a text (no IO)
parseConllu parser text =
    case r of
            Left err -> Left (s2t $ M.parseErrorPretty err)
            Right ss -> Right   ss
    where
        r = M.parse parser "" (t2s text)   -- why a textname required ??

prettyPrintConlluSentence ::T.Sentence -> Text
-- | prettyprint a single sentence
prettyPrintConlluSentence s = s2t . Pr.fromDiffList . Pr.printSent $ s

-- convert to Doc1
instance  ConvertTo1 UD.POStag  [T.Sentence] (Doc1 UD.POStag) where
    convertTo1 postag lang sents = Doc1 {..}
        where
            doc1sents = zipWith  (convertTo1sentence  postag lang) [1..] sents
            -- needs numbering of sentences
            doc1corefs = Nothing

convertTo1sentence :: UD.POStag -> LanguageCode -> Int -> T.Sentence -> Sentence1 UD.POStag
--instance ConvertTo1 postag  T.Sentence (Sentence1 postag) where
convertTo1sentence  postag lang i T.Sentence {..} = Sentence1 {..}
        where
            s1id = SentenceID i  -- must start with 1 to conform with coreNLP in general
            s1parse = Nothing
            s1toks = map (convertTo1 postag lang) _tokens
            s1deps = Just $ map (convertTo1deps postag lang) _tokens
                    -- extract the dependencies from the tokens
            s1entitymentions = Nothing

instance  ConvertTo1 UD.POStag  T.Token (Token0 UD.POStag) where
    convertTo1 _ lang T.SToken {..} = Token0 {..}
      where
        tid = TokenID  _ix
        tword = maybe zero   (\l -> Wordform0 $ LCtext  (s2t l) lang) _form
        twordOrig = Nothing
--        if tok_word == tok_originalText then Nothing else Just tok_originalText
        tlemma = maybe zero (\l -> Lemma0 $ LCtext (s2t l) lang) _lemma
        tpos = maybe UD.tagUNK (\p -> ( NLP.parseTag  . showT $ p ) )   _upostag
        tfeature = _feats
        tposOrig = Just . showT $ _upostag
--        if showT pos == tok_pos then Nothing else Just tok_pos
        -- missig a test that parse was complete
        tpostt = Nothing
        tner = []
--        parseNERtagList [tok_ner] -- when is this a list?
        tnerOrig = Nothing
--            if (any isAnUnknownNER $ tner) then Just [tok_ner] else Nothing
                        -- use the Ner2 values?
        tspeaker = []
--            parseSpeakerTagList . maybeToList $ tok_speaker
--                    maybe [] (\a -> [a]) $ tok_speaker
        tbegin = zero -- tok_characterOffsetBegin
        tend = zero --  tok_characterOffsetEnd
        tbefore = Just $ if tpos == UD.PUNCT then "" else " " -- tok_before
        tafter = Just "" -- tok_after

convertTo1deps :: postag -> LanguageCode -> T.Token -> Dependence1
convertTo1deps _ lang T.SToken {..} = Dependence1 {..}
        where
            d1type = maybe (DepUnknown $ showT _deprel) (id)  _deprel  :: DepCode
            d1orig = zero -- dep_dep
            d1govid = maybe zero TokenID _dephead
            d1depid = TokenID _ix
            d1govGloss = zero -- LCtext {ltxt = dep_governorGloss, llang = lang}
            d1depGloss = maybe zero   (\l ->  LCtext  (s2t l) lang) _form


test_read3 =   do
    res   <- runErr $ do
        let fn = "veryshort1ud.txt" :: FilePath
        putIOwords ["read a udfeat file and parse"]
        in1 :: Text <- readFile2 (makeRelFile fn)
        putIOwords ["read short1ud.txt", in1]

        let res1 = parseConllu (P.documentC P.sentence) in1  :: ErrOrVal [T.Sentence]

        let res2 = case res1 of
                    Right ss -> unlines' $ map showT ss
                    Left msg -> errorT [msg]
        putIOwords ["result from parse ", res2]
        let ret2 = concat' $ either singleton (map prettyPrintConlluSentence) res1
        putIOwords ["result from parse pretty ", ret2]

--        let res3 = either (error . t2s) id  res1  :: [T.Sentence]
        let res4 = fromRightEOV res1 ::[T.Sentence]

        let doc1 = convertTo1 (UD.undefUPOS::UD.POStag) English res4  :: Doc1 UD.POStag

        putIOwords ["converted to doc1 format", s2t $ ppShow doc1]

--        let doc3 = to1op doc1
        let doc4 = to11opUD doc1   -- absolute
--        putIOwords ["converted to absolute format", s2t $ ppShow doc4]
        let docLin5 = toLinUD doc4   -- linear
--        putIOwords ["converted to linear format", s2t $ ppShow docLin5]
        let docTrip6 = toTripleUD docLin5 -- triples
        putIOwords ["converted to Triple format", s2t $ ppShow docTrip6]
        let docNT = toNT docTrip6  -- NT
        putIOwords ["converted to NT format", docNT]

----        res1 <- callIO $  readConllu "veryshort1ud_copy).txt"
--        res1 <- callIO $  readConllu fn
--        putIOwords ["result from readConllu", showT res1]
--        let ret1 = map printDoc res1
--        let ret2 = s2t .  concat  $ ret1
--        putIOwords ["should be the same as input"
--                , ret2]
--
--
        return ("","X")
    case res of
        Left msg -> do
            putIOwords ["error was", msg]
            assertBool False
        Right (exp, ret) -> do
            assertEqual exp ret

--singleton s = [s]

{-
test_read1 = do
    res1 <- readConllu "veryshort1ud.txt"
    putIOwords ["result from readConllu", showT res1]
    assertBool False


test_read2 = do
    res   <- runErr $ do
        let fn = "veryshort1ud.txt" :: FilePath
        putIOwords ["read a udfeat file and parse"]
        in1 :: Text <- readFile2 (makeRelFile fn)
        putIOwords ["read short1ud.txt", in1]
--        res1 <- callIO $  readConllu "veryshort1ud_copy).txt"
        res1 <- callIO $  readConllu fn
        putIOwords ["result from readConllu", showT res1]
        let ret1 = map printDoc res1
        let ret2 = s2t .  concat  $ ret1
        putIOwords ["should be the same as input"
                , ret2]


        return (in1, ret2)
    case res of
        Left msg -> do
            putIOwords ["error was", msg]
            assertBool False
        Right (exp, ret) -> do
            assertEqual exp ret

-}

    --
