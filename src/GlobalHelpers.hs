{-# LANGUAGE OverloadedStrings #-}

module GlobalHelpers
  ( formatLinks
  , formatEmail
  , formatCv
  ) where

import Types (Link(..))
import qualified Data.Text as T

formatLinks :: [Link] -> T.Text
formatLinks =
  T.intercalate " | "
    . map (\(Link n h) ->
             "<a href=\"" <> h <> "\">" <> n <> "</a>")

formatEmail :: Maybe T.Text -> T.Text
formatEmail email =
  case email of
    Just e  -> "<a href=\"mailto:" <> e <> "\">" <> e <> "</a> | "
    Nothing -> ""

formatCv :: Maybe FilePath -> T.Text
formatCv cvSlug =
  case cvSlug of
    Just cv -> " | <a href=\"../cv/" <> T.pack cv <> "\">CV</a>"
    Nothing -> ""
