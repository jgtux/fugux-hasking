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
  { slug  :: FilePath      -- output HTML file name
  , name  :: T.Text        -- site name
  , email :: Maybe T.Text  -- optional email
  , links :: [T.Text]      -- social links
  }

-- | Generate SEO-friendly index.html with articles and social links
genArticleMenu :: Site -> [A.Article] -> IO ()
genArticleMenu site articles = do
    createDirectoryIfMissing True "main"

    -- Meta description from first few article excerpts
    let metaDescription = T.take 160 $ T.intercalate " "
          [ maybe "" stripHtml (A.excerpt a) | a <- take 5 articles ]

        -- Article list items
        articleLinks = T.concat
          [ "<li><article>\n"
            <> "<h2><a href=\"../articles/" <> T.pack (A.slug a) <> "\">"
            <> stripHtml (A.title a) <> "</a></h2>\n"
            <> "<time datetime=\"" <> dateText (A.date a) <> "\">"
            <> dateText (A.date a) <> "</time>\n"
            <> "<p class=\"excerpt\">" <> maybe "" stripHtml (A.excerpt a) <> "</p>\n"
            <> "</article></li>\n"
          | a <- articles
          ]
        -- Social links in a <ul> if any
        socialLinksHtml = if null (links site)
                          then ""
                          else "<nav aria-label=\"Social Media\">\n<ul class=\"social-links\">\n"
                               <> T.concat [ "<li><a href=\"" <> link <> "\">" <> link <> "</a></li>\n"
                                           | link <- links site ]
                               <> "</ul>\n</nav>\n"

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
          , socialLinksHtml
          , "<main>\n<section>\n<h2>Articles</h2>\n<ul>\n"
          , articleLinks
          , "</ul>\n</section>\n</main>\n"
          , "<footer>\n<p>&copy; 2025 Fugux.</p>\n"
          , "</footer>\n"
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
