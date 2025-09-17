{-# LANGUAGE OverloadedStrings #-}

module Article
  ( Article(..)
  , saveArticleHTML
  ) where

import Text.Pandoc
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Highlighting (Style, espresso)
import Text.Pandoc.Walk (query)
import Text.Parsec
import Text.Parsec.Text (Parser)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Time (UTCTime)
import System.FilePath (takeFileName, replaceExtension, (</>))
import qualified System.Directory as DIR (createDirectoryIfMissing, getModificationTime)
import Types (Article(..), Site(..), Link(..))
import GlobalHelpers (formatLinks, formatEmail)


saveArticleHTML :: Site -> FilePath -> IO (Either PandocError Article)
saveArticleHTML site path = runIO $ do
    let baseExts =
          [ Ext_hard_line_breaks
          , Ext_fancy_lists
          , Ext_tex_math_dollars
          , Ext_tex_math_double_backslash
          , Ext_backtick_code_blocks
          , Ext_fenced_code_blocks
          , Ext_fenced_code_attributes
          , Ext_header_attributes
          , Ext_bracketed_spans
          , Ext_fenced_divs
          ]

        readerExts = foldr enableExtension (readerExtensions def) baseExts
        writerExts = foldr enableExtension (writerExtensions def) baseExts
        readerOpts = def { readerExtensions = readerExts }
        writerOpts = def { writerExtensions = writerExts, writerHTMLMathMethod = MathML }

    doc <- readMarkdown readerOpts =<< (liftIO $ TIO.readFile path)

    titleHtml <- case query getFirstH1 doc of
        (block:_) -> writeHtml5String def (Pandoc nullMeta [block])
        []        -> return "Untitled"

    excerptHtml <- case query getFirstParagraph doc of
        (block:_) -> Just <$> writeHtml5String def (Pandoc nullMeta [block])
        []        -> return Nothing

    articleHtml <- writeHtml5String writerOpts doc

    let plainTitle = T.concat $ query getStrH1 doc
        metaDescription = maybe "" (T.replace "\n" "" . T.replace "<br>" " " . T.replace "<br />" " " . T.replace "<p>" "" . T.replace "</p>" "" . T.strip) excerptHtml

        htmlTemplate :: T.Text
        htmlTemplate = 
          "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"UTF-8\">\n"
          <> "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
          <> "<title>" <> plainTitle <> "</title>\n"
          <> "<meta name=\"description\" content=\"" <> metaDescription <> "\">\n"
          <> "<meta property=\"og:title\" content=\"" <> plainTitle <> "\">\n"
          <> "<meta property=\"og:description\" content=\"" <> metaDescription <> "\">\n"
          <> "<meta property=\"og:type\" content=\"article\">\n"
          <> "<link rel=\"stylesheet\" href=\"../main/style.css\">\n"
          <> "</head>\n<body>\n"
          <> "<header>" <> titleHtml <> "</header>\n"
          <> "<main>\n<article>\n"
          <> removeH1 articleHtml
          <> "\n</article>\n</main>\n"
          <> "<footer>\n<address>" <> "João G. — " <> formatEmail (email site) <> formatLinks (links site) <> "</address>\n<p>&copy; 2025 Fugux.</p>\n</footer>\n"
          <> "</body>\n</html>"
          
        baseName = replaceExtension (takeFileName path) "html"
        outDir   = (dir site) </> "articles"
        outPath  = outDir </> baseName

    liftIO $ DIR.createDirectoryIfMissing True outDir
    liftIO $ TIO.writeFile outPath htmlTemplate

    utcLastUpdated  <- liftIO $ DIR.getModificationTime path

    return $ Article
        { articleSlug      = baseName
        , title     = titleHtml
        , utcTime   = utcLastUpdated
        , excerpt   = excerptHtml
        }

getFirstH1 :: Block -> [Block]
getFirstH1 h@(Header 1 _ _) = [h]
getFirstH1 _                = []

getFirstParagraph :: Block -> [Block]
getFirstParagraph p@(Para _) = [p]
getFirstParagraph _          = []

getStrH1 :: Block -> [T.Text]
getStrH1 (Header 1 _ inlines) = [T.concat $ map go inlines]
  where
    go (Str t)    = t
    go Space      = " "
    go SoftBreak  = " "
    go LineBreak  = " "
    go (Code _ t) = t
    go _          = ""
getStrH1 _ = []

removeH1 :: T.Text -> T.Text
removeH1 html = case parse parser "" html of
                  Left _ -> html
                  Right result -> result
  where
    parser :: Parser T.Text
    parser = T.concat <$> many (try h1 <|> fmap T.singleton anyChar)

    h1 :: Parser T.Text
    h1 = do
      try $ string "<h1"
      manyTill anyChar (char '>')
      manyTill anyChar (try $ string "</h1>")
      return T.empty
