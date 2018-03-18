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


test_read3 =   do
    res   <- runErr $ do
        let fn = "veryshort1ud.txt" :: FilePath
        putIOwords ["read a udfeat file and parse"]
        in1 :: Text <- readFile2 (makeRelFile fn)
        putIOwords ["read short1ud.txt", in1]

        let res1 = parseConllu (P.documentC P.sentence) in1

        let res2 = case res1 of
                    Right ss -> unlines' $ map showT ss
                    Left msg -> msg
        putIOwords ["result from parse ", res2]
        let ret2 = concat' $ either singleton (map prettyPrintConlluSentence) res1
        putIOwords ["result from parse pretty ", ret2]

----        res1 <- callIO $  readConllu "veryshort1ud_copy).txt"
--        res1 <- callIO $  readConllu fn
--        putIOwords ["result from readConllu", showT res1]
--        let ret1 = map printDoc res1
--        let ret2 = s2t .  concat  $ ret1
--        putIOwords ["should be the same as input"
--                , ret2]
--
--
        return (in1, ret2)
    case res of
        Left msg -> do
            putIOwords ["error was", msg]
            assertBool False
        Right (exp, ret) -> do
            assertEqual exp ret

singleton s = [s]

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
