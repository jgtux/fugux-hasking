{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Site(..)
  , Link(..)
  , Article(..)
  ) where

import Data.Text as T
import Data.Time as TM (UTCTime)

data Link = Link
  { refName :: T.Text 
  , href :: T.Text
  }
  
data Site = Site
  { slug  :: FilePath      -- output HTML file name
  , dir   :: FilePath      -- directory 
  , name  :: T.Text        -- site name
  , email :: Maybe T.Text  -- optional email
  , links :: [Link] -- social media name and link refering to it
  , cvSlug :: Maybe FilePath -- curriculum vitae optional
  }

data Article = Article
  { articleSlug      :: FilePath
  , title     :: T.Text
  , utcTime   :: TM.UTCTime
  , excerpt   :: Maybe T.Text
  } 

