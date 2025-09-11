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
import Data.Time (Day)
import System.FilePath (takeFileName, replaceExtension, (</>))
import System.Directory (createDirectoryIfMissing, copyFile)

data Article = Article
  { slug    :: FilePath
  , title   :: T.Text
  , date    :: Day
  , excerpt :: Maybe T.Text
  } deriving (Show)

saveArticleHTML :: Day -> FilePath -> IO (Either PandocError Article)
saveArticleHTML currentDate path = runIO $ do
    -- Opts

    let baseExts =
          [ Ext_hard_line_breaks
          , Ext_fancy_lists
          , Ext_tex_math_dollars
          , Ext_tex_math_double_backslash
          , Ext_backtick_code_blocks
          , Ext_fenced_code_blocks
          ]


    let readerExts = foldr enableExtension (readerExtensions def) baseExts
        writerExts = foldr enableExtension (writerExtensions def) baseExts

    let readerOpts = def { readerExtensions = readerExts }
        writerOpts = def { writerExtensions = writerExts, writerHTMLMathMethod = MathML, writerHighlightStyle = Just espresso }

    -- Read Markdown
    -- content <- liftIO $ TIO.readFile path
    -- doc <- readMarkdown readerOpts content
    doc <- readMarkdown readerOpts =<< (liftIO $ TIO.readFile path)

    
    -- Extract metadata
    titleHtml <- case query getFirstHeading doc of
        (block:_) -> writeHtml5String def (Pandoc nullMeta [block])
        []        -> return "Untitled"

    excerptHtml <- case query getFirstParagraph doc of
        (block:_) -> Just <$> writeHtml5String def (Pandoc nullMeta [block])
        []        -> return Nothing

    -- Render body HTML
    articleHtml <- writeHtml5String writerOpts doc

    -- Plain title
    let plainTitle = T.strip $ T.filter (/= '\n') (T.concat $ T.words $ T.replace "<h1>" "" $ T.replace "</h1>" "" titleHtml)

    -- Build meta description for excerpt
    let metaDescription = maybe "" (T.replace "<br />" "" . T.replace "<p>" "" . T.replace "</p>" "" . T.strip) excerptHtml

    
    
    -- Prepare full HTML manually
    let htmlTemplate = T.concat
          [ "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"UTF-8\">\n"
          , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
          , "<title>", plainTitle, "</title>\n"
          , "<meta name=\"description\" content=\"", metaDescription, "\">\n"
          , "<meta property=\"og:title\" content=\"", plainTitle, "\">\n"
          , "<meta property=\"og:description\" content=\"", metaDescription, "\">\n"
          , "<meta property=\"og:type\" content=\"article\">\n"
          , "<link rel=\"stylesheet\" href=\"../main/style.css\">\n"
          , "</head>\n<body>\n"
          , "<header>", titleHtml, "</header>\n"
          , "<main>\n<article>\n"
          , cleanParaAndAllocateCodeInPre $ removeH1 articleHtml
          , "\n</article>\n</main>\n"
          , "<footer><p>&copy; 2025 Fugux.</p></footer>\n"
          , "</body>\n</html>"
          ]

    -- Prepare output path
    let baseName = replaceExtension (takeFileName path) "html"
        outDir   = "articles"
        outPath  = outDir </> baseName

    liftIO $ createDirectoryIfMissing True outDir
    -- Optionally copy style.css to articles/ if you want local copy
    -- liftIO $ copyFile "main/style.css" (outDir </> "style.css")
    liftIO $ TIO.writeFile outPath htmlTemplate

    -- Return Article metadata
    return $ Article
        { slug    = baseName
        , title   = titleHtml
        , date    = currentDate
        , excerpt = excerptHtml
        }

-- Helpers
getFirstHeading :: Block -> [Block]
getFirstHeading h@(Header _ _ _) = [h]
getFirstHeading _                = []

getFirstParagraph :: Block -> [Block]
getFirstParagraph p@(Para _) = [p]
getFirstParagraph _          = []

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

cleanParaAndAllocateCodeInPre :: T.Text -> T.Text
cleanParaAndAllocateCodeInPre html = case parse parser "" html of
                           Left _      -> html
                           Right result -> result
  where
    parser :: Parser T.Text
    parser = T.concat <$> many (try codeBlock <|> fmap T.singleton anyChar)

    codeBlock :: Parser T.Text
    codeBlock = do
      optional $ try $ string "<p>"
      try $ string "<code>"
      content <- manyTill anyChar (try $ string "</code>")
      _ <- string "</pre>" <|> pure ""  
      optional $ try $ string "</p>"
      return $ "<pre><code>" <> T.pack content <> "</code></pre>"
