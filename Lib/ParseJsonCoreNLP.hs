-----------------------------------------------------------------------------
--
-- Module      :  parsing the output of stanford corenlp 3.9. in json format
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE DeriveGeneric #-}

module Lib.ParseJsonCoreNLP -- (openMain, htf_thisModuelsTests)
     where

import           Uniform.Strings
import Uniform.FileIO

import Data.Aeson
import Data.Aeson.Types  -- for modifying the labels
import GHC.Generics
--import qualified Data.ByteString.Lazy as B
import Control.Monad (mzero)

parseNLP :: ErrIO ()
parseNLP = do
    f :: LazyByteString <- readFile2 (makeRelFile "short1 .json")
    let r = decode f  :: Maybe [FlickrResponse]
    putIOwords ["decoded", showT r]
    return ()

decodeFlickrResponse :: LazyByteString -> Maybe [FlickrResponse]
decodeFlickrResponse = decode

data Photo = Photo
    { photo_id   :: Text
    , photo_secret    :: Text
    , photo_server    :: Text
    , photo_farm      :: Int
    , photo_title     :: Text
    , photo_isprimary :: Text
    , photo_ispublic  :: Int
    , photo_isfriend  :: Int
    , photo_isfamily  :: Int
    } deriving (Show, Generic, Eq)

data Photoset = Photoset
    { set_id :: Text
    , set_primary    :: Text
    , set_owner      :: Text
    , set_ownername  :: Text
    , set_photo      :: [Photo]
    , set_page       :: Int
    , set_per_page   :: Int
    , set_perpage    :: Int
    , set_pages      :: Int
    , set_total      :: Text
    , set_title       :: Text
    } deriving (Show, Generic, Eq)


data FlickrResponse = FlickrResponse
    { f_photoset :: Photoset
    , f_stat     :: Text
    } deriving (Show, Generic, Eq)

instance FromJSON Photo where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 6 }
instance FromJSON Photoset where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 4 }
instance FromJSON FlickrResponse where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 2 }

--instance FromJSON Photo where
--    parseJSON (Object v) =
--        Photo <$> v .: "id"
--              <*> v .: "secret"
--              <*> v .: "server"
--              <*> v .: "farm"
--              <*> v .: "title"
--              <*> v .: "isprimary"
--              <*> v .: "ispublic"
--              <*> v .: "isfriend"
--              <*> v .: "isfamily"
--    parseJSON _ = mzero

--instance FromJSON Photoset where
--    parseJSON (Object o) =
--        Photoset <$> o .: "id"
--                 <*> o .: "primary"
--                 <*> o .: "owner"
--                 <*> o .: "ownername"
--                 <*> o .: "photo"
--                 <*> o .: "page"
--                 <*> o .: "per_page"
--                 <*> o .: "perpage"
--                 <*> o .: "pages"
--                 <*> o .: "total"
--                 <*> o .: "title"
--    parseJSON _ = mzero

--instance FromJSON FlickrResponse where
--    parseJSON (Object v) =
--        FlickrResponse <$> v .: "photoset"
--                       <*> v .: "stat"



--readBSlazy :: FilePath ->  IO B.ByteString
--readBSlazy  =  B.readFile




