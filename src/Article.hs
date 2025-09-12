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
          , Ext_fenced_code_attributes     -- <- important!
          , Ext_header_attributes          -- headings {#id .class}
          , Ext_bracketed_spans            -- [text]{.class}
          , Ext_fenced_divs                -- ::: {.class}
          ]


    let readerExts = foldr enableExtension (readerExtensions def) baseExts
        writerExts = foldr enableExtension (writerExtensions def) baseExts

    let readerOpts = def { readerExtensions = readerExts }
        writerOpts = def { writerExtensions = writerExts, writerHTMLMathMethod = MathML }

    -- Read Markdown
    -- content <- liftIO $ TIO.readFile path
    -- doc <- readMarkdown readerOpts content
    doc <- readMarkdown readerOpts =<< (liftIO $ TIO.readFile path)

    
    -- Extract metadata
    titleHtml <- case query getFirstH1 doc of
        (block:_) -> writeHtml5String def (Pandoc nullMeta [block])
        []        -> return "Untitled"

    excerptHtml <- case query getFirstParagraph doc of
        (block:_) -> Just <$> writeHtml5String def (Pandoc nullMeta [block])
        []        -> return Nothing

    -- Render body HTML
    articleHtml <- writeHtml5String writerOpts doc

    -- Plain title
    let plainTitle = T.concat $ query getStrH1 doc

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
          , removeH1 articleHtml
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
getFirstH1 :: Block -> [Block]
getFirstH1 h@(Header 1 _ _) = [h]
getFirstH1 _                = []

getFirstParagraph :: Block -> [Block]
getFirstParagraph p@(Para _) = [p]
getFirstParagraph _          = []

getStrH1 :: Block -> [T.Text]
getStrH1 (Header 1 _ inlines) = [T.concat $ map go inlines]
  where
    go :: Inline -> T.Text
    go (Str t)      = t
    go Space        = " "
    go SoftBreak    = " "
    go LineBreak    = " "
    go (Code _ t)   = t
    go _            = ""
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

