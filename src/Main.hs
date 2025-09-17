{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Time as Time
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import qualified Site as SITE
import qualified Article as ARTCL
import Types (Site(..), Link(..))

main :: IO ()
main = do
    -- Site metadata
    let site = Site
          { slug      = "index.html"
          , dir       = "/home/wizardfag/my-web-site"
          , name      = "fugux"
          , email     = Just "jg@fugu.mail"
          , links     =
            [ Link "GitHub"   "https://github.com/jgtux"
            , Link "LinkedIn" "https://www.linkedin.com/in/jv-guedes-unixer"
            ]
          }

    -- Collect Markdown files from "content/"
    files <- listDirectory "content"
    let mdFiles = [ "content" </> f | f <- files, takeExtension f == ".md" ]

    -- Convert all Markdown → Article
    articlesE <- mapM (\mdFile -> ARTCL.saveArticleHTML site mdFile) mdFiles

    -- Filter successful conversions
    let articles = [a | Right a <- articlesE]

    -- Generate index.html
    SITE.genArticleMenu site articles

    putStrLn "✅ Site generated in ./main and ./articles"
