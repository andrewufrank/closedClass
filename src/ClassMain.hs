-----------------------------------------------------------------------------
--
-- Module      :   main for tests
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- {-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards
    #-}
-- {-# LANGUAGE DeriveGeneric #-}

module Main     where      -- must have Main (main) or Main where

--import Uniform.FileIO
--import Data.RDF.FileTypes
--import System.Exit

--import           ClosedClass
--import           Lib.BlogExample
--import  Lib.Tutorial1
--import Lib.ParseJsonCoreNLP
--import Lib.Doc2ToDoc0
--import Lib.ProduceNLPtriples2
--
--import Uniform.Convenience.StartApp
--import qualified NLP.Corpora.Conll  as Conll
--
--import Data.Aeson (eitherDecode)
--import Parser.LanguageTypedText (undefEnglish)
--import Parser.ProduceNLPtriples (unNLPtriple)
--pimport Parser.NLPvocabulary (ParaSigl (..), SnipID (..), mkSnipSigl)
--import Data.RDF.Extension
--import Producer.Servers -- (PartURI)
--import Lib.DerivingExample
import Lib.DerivingMinimalExample
main :: IO ()
main =  do  -- with tests in other modules
        deriveTest
        -- insert here modules in IO




