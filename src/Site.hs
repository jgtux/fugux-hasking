{-# LANGUAGE OverloadedStrings #-}

module Site
  ( Site(..)
  , genArticleMenu
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import qualified Article as A (Article(..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time (Day)

data Site = Site
  { slug  :: FilePath
  , name  :: T.Text
  , email :: Maybe T.Text
  , links :: [T.Text]
  }

-- | Generate SEO-friendly index.html with links to all articles
genArticleMenu :: Site -> [A.Article] -> IO ()
genArticleMenu site articles = do
    createDirectoryIfMissing True "main"

    -- Build meta description from first few articles
    let metaDescription = T.take 160 $ T.intercalate " "
          [ maybe "" stripHtml (A.excerpt a) | a <- take 5 articles ]

        articleLinks = T.concat
          [ "<li><article>\n"
          <> "<h2><a href=\"../articles/" <> T.pack (A.slug a) <> "\">"
          <> stripHtml (A.title a) <> "</a></h2>\n"
          <> "<p>" <> maybe "" stripHtml (A.excerpt a) <> "</p>\n"
          <> "<time datetime=\"" <> dateText (A.date a) <> "\">" <> dateText (A.date a) <> "</time>\n"
          <> "</article></li>\n"
          | a <- articles
          ]

        htmlTemplate = T.concat
          [ "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"UTF-8\">\n"
          , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
          , "<title>", name site, "</title>\n"
          , "<meta name=\"description\" content=\"", metaDescription, "\">\n"
          , "<meta property=\"og:title\" content=\"", name site, "\">\n"
          , "<meta property=\"og:description\" content=\"", metaDescription, "\">\n"
          , "<meta property=\"og:type\" content=\"website\">\n"
          , "<link rel=\"stylesheet\" href=\"style.css\">\n"
          , "</head>\n<body>\n"
          , "<header><h1>", name site, "</h1></header>\n"
          , "<main>\n<section>\n<h2>Articles</h2>\n<ul>\n"
          , articleLinks
          , "</ul>\n</section>\n</main>\n"
          , "<footer><p>&copy; 2025 Fugux.</p></footer>\n"
          , "</body>\n</html>"
          ]

    TIO.writeFile ("main" </> slug site) htmlTemplate

-- Helpers

-- Format Day as Text
dateText :: Day -> T.Text
dateText = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

-- Strip basic HTML tags for meta description / title
stripHtml :: T.Text -> T.Text
stripHtml = T.concat . go
  where
    go txt
      | T.null txt = []
      | "<" `T.isPrefixOf` txt =
          let rest = T.drop 1 $ T.dropWhile (/= '>') txt
          in go rest
      | otherwise =
          let (chunk, rest) = T.breakOn "<" txt
          in chunk : go rest
