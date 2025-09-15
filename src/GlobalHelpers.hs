{-# LANGUAGE OverloadedStrings #-}

module GlobalHelpers
  ( formatLinks
  , formatEmail
  ) where

import Types (Link(..))
import qualified Data.Text as T

formatLinks :: [Link] -> T.Text
formatLinks =
  T.intercalate " — "
    . map (\(Link n h) ->
             "<a href=\"" <> h <> "\">" <> n <> "</a>")

formatEmail :: Maybe T.Text -> T.Text
formatEmail email =
  case email of
    Just e  -> "<a href=\"mailto:" <> e <> "\">" <> e <> "</a> — "
    Nothing -> ""
