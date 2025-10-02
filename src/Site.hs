{-# LANGUAGE OverloadedStrings #-}

module Site
  ( genArticleMenu
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sortOn)
import Data.Ord  (Down(..))
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Data.Time (UTCTime, formatTime, defaultTimeLocale) 
import Types (Site(..), Article(..))
import GlobalHelpers (formatCv, formatLinks, formatEmail)

-- Generate SEO-friendly index.html with articles and social links
genArticleMenu :: Site -> [Article] -> IO ()
genArticleMenu site articles = do
    -- Ensure 'main' folder exists inside site dir
    let mainDir = dir site </> "main"
    createDirectoryIfMissing True mainDir

    -- Ensure 'cv' folder exists inside site dir
    let cvDir = dir site </> "cv"
    createDirectoryIfMissing True cvDir

    -- Ensure empty style.css exists inside 'main'
    let cssPath = mainDir </> "style.css"
    cssExists <- doesFileExist cssPath
    if not cssExists
       then TIO.writeFile cssPath "" 
       else return ()

    -- Meta description from first few article excerpts
    let metaDescription =
          T.take 160 $
          T.intercalate " "
          [ maybe "" (T.strip
                      . T.replace "\n" ""
                      . T.replace "<br>" " "
                      . T.replace "<br />" " "
                      . T.replace "<p>" ""
                      . T.replace "</p>" ""
                     ) (excerpt a)
          | a <- articles
          ]    

        -- Article list items, sorted by utcTime descending
        articleLinks =
           [ "<li><article>\n"
             <> "<h2><a href=\"../articles/" <> T.pack (articleSlug a) <> "\">"
             <> stripHtml (title a) <> "</a></h2>\n"
             <> "<time datetime=\"" <> isoDateTime (utcTime a) <> "\">"
             <> dateUtcText (utcTime a) <> " " <> utcTimeText (utcTime a) <> "</time>\n"
             <> "</article></li>\n"
           | a <- sortOn (Down . utcTime) articles
           ]

        -- Social links
        socialLinksHtml
          | null (links site) && email site == Nothing = ""
          | otherwise = "<address>"
                    <> formatEmail (email site)
                    <> formatLinks (links site)
                    <> formatCv (cvSlug site) 
                    <> "<br>Have a request? Contact me.</address>"

        htmlTemplate = 
           "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"UTF-8\">\n"
           <> "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
           <> "<title>" <> name site <> "</title>\n"
           <> "<meta name=\"description\" content=\"" <> metaDescription <> "\">\n"
           <> "<meta property=\"og:title\" content=\"" <> name site <> "\">\n"
           <> "<meta property=\"og:description\" content=\"" <> metaDescription <> "\">\n"
           <> "<meta property=\"og:type\" content=\"website\">\n"
           <> "<link rel=\"stylesheet\" href=\"style.css\">\n"
           <> "</head>\n<body>\n"
           <> "<header><h1>" <> name site <> "</h1></header>\n"
           <> socialLinksHtml
           <> "<main>\n<section>\n<h2>Articles</h2>\n<ul>\n"
           <> T.concat articleLinks
           <> "</ul>\n</section>\n</main>\n"
           <> "<footer>\n<address> </address><p>&copy; 2025 fugu.cafe</p>\n"
           <> "</footer>\n"
           <> "</body>\n</html>"

    TIO.writeFile (mainDir </> slug site) htmlTemplate


-- Helpers

-- Time as Text
utcTimeText :: UTCTime -> T.Text
utcTimeText = T.pack . formatTime defaultTimeLocale "%H:%M %Z"

isoDateTime :: UTCTime -> T.Text
isoDateTime = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"

-- Format Day as Text
dateUtcText :: UTCTime -> T.Text
dateUtcText = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

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
