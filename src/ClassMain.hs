-----------------------------------------------------------------------------
--
-- Module      :   main for tests
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
--{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards
    #-}
--{-# LANGUAGE DeriveGeneric #-}

module Main     where      -- must have Main (main) or Main where

import Uniform.FileIO
import Data.RDF.FileTypes
--import System.Exit

--import           ClosedClass
--import           Lib.BlogExample
--import  Lib.Tutorial1
import Lib.ParseJsonCoreNLP
import Lib.Doc2ToDoc0
import Lib.ProduceNLPtriples2

import Uniform.Convenience.StartApp
import qualified NLP.Corpora.Conll  as Conll

import Data.Aeson (eitherDecode)
import Parser.LanguageTypedText (undefEnglish)
import Parser.ProduceNLPtriples (unNLPtriple)
import Parser.NLPvocabulary (ParaSigl (..), SnipID (..), mkSnipSigl)
import Data.RDF.Extension
import Producer.Servers -- (PartURI)

main :: IO ()
main =  do  -- with tests in other modules
        main2
        -- insert here modules in IO
--    tutorial1Main
--    blogMain
--    closedMain
--    blogMain

programName = "ClassMain - from SomeTest"
progTitle = "test Example"

main2 :: IO ()
main2 = startProg programName   progTitle
    (do
--        tutorial1Main
--        blogMain
        main4tripels
    )


main4tripels :: ErrIO ()
main4tripels = do
        let fn = makeRelFile "short1.json"
        putIOwords ["nlp json decode:", showT fn]
        f <- readFile2  fn
--        putIOwords ["json input:",showStartJson f]
        let r = eitherDecode  f  :: Either String Doc2
--        let doc2 :: Doc2
        doc2 :: Doc2 <- case r of
                Left msg ->  throwErrorT ["doc2 left",  s2t msg]
                Right a -> return a
        putIOwords ["doc2:",  showT doc2, "\n\n\n"]
        let doc1 = doc2to1 Conll.undefConll doc2
        putIOwords ["doc1:", showT doc1, "\n\n\n"]
        let trip = processDoc1toTriples2 undefEnglish Conll.undefConll
                sigl1 doc1
        putIOwords ["trips", showT  trip]
        let succ = (showT trip == resTrip)
        putIOwords ["success", showT succ]
        let fp = (makeAbsFile "/home/frank/Workspace8/testSome/triples")
        hand <- openHandle6 fp ntFileTriples
        let trip2 = map unNLPtriple trip
        writeHandle6 hand ntFileTriples trip2
        closeHandle6 fp ntFileTriples hand

        return ()

resTrip = ""

paraSigl1 =  ParaSigl ( extendSlashRDFsubj "produceDocCallNLP"
        (RDFsubj $ (unPartURI rdfBase))  )
sigl1 = mkSnipSigl paraSigl1 (SnipID 1)

