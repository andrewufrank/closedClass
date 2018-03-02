-----------------------------------------------------------------------------
--
-- Module      :  checking the code for reading nested json from blog
-- http://blog.gnclmorais.com/aeson-and-nested-json
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE DeriveGeneric #-}

module Lib.BlogExample  -- (openMain, htf_thisModuelsTests)
     where

import           Uniform.Strings

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Control.Monad (mzero)

blogMain :: IO ()
blogMain = do
    f <- getJSON
    let r = decode f  :: Maybe [FlickrResponse]
    putIOwords ["decoded", showT r]
    return ()

data Photo = Photo
    { id   :: Text
    , secret    :: Text
    , server    :: Text
    , farm      :: Int
    , title     :: Text
    , isprimary :: Text
    , ispublic  :: Int
    , isfriend  :: Int
    , isfamily  :: Int
    } deriving Show

data Photoset = Photoset
    { psid :: Text
    , primary    :: Text
    , owner      :: Text
    , ownername  :: Text
    , photo      :: [Photo]
    , page       :: Int
    , per_page   :: Int
    , perpage    :: Int
    , pages      :: Int
    , total      :: Text
    , name       :: Text
    } deriving Show

data FlickrResponse = FlickrResponse
    { photoset :: Photoset
    , stat     :: Text
    } deriving Show

-- for generics:
--instance FromJSON Photo
--instance FromJSON Photoset
--instance FromJSON FlickrResponse

instance FromJSON Photo where
    parseJSON (Object v) =
        Photo <$> v .: "id"
              <*> v .: "secret"
              <*> v .: "server"
              <*> v .: "farm"
              <*> v .: "title"
              <*> v .: "isprimary"
              <*> v .: "ispublic"
              <*> v .: "isfriend"
              <*> v .: "isfamily"
    parseJSON _ = mzero

instance FromJSON Photoset where
    parseJSON (Object o) =
        Photoset <$> o .: "id"
                 <*> o .: "primary"
                 <*> o .: "owner"
                 <*> o .: "ownername"
                 <*> o .: "photo"
                 <*> o .: "page"
                 <*> o .: "per_page"
                 <*> o .: "perpage"
                 <*> o .: "pages"
                 <*> o .: "total"
                 <*> o .: "title"
    parseJSON _ = mzero

instance FromJSON FlickrResponse where
    parseJSON (Object v) =
        FlickrResponse <$> v .: "photoset"
                       <*> v .: "stat"


jsonFile :: FilePath
jsonFile = "blog.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile




