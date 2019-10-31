{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Html
( col
, link
, linkDecode
, WebResult(..)) where

import Control.Applicative
import Control.Monad.State
import qualified Data.CaseInsensitive as CI
import Data.Function
import Data.List (intersperse)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String (IsString(..))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Debug.Trace as TR
import Network.HTTP.Types.Status (ok200, found302)
import Network.URI.Encode as ENC
import System.Directory (copyFile)
import System.IO
import Text.Pretty.Simple (pShow)
import Web.Firefly

import Util 

-- name contents attributes
data HTML = HTMLString Text | HTMLPair HTML HTML | HTMLNothing
  deriving Show
htmlRender :: HTML -> Text
htmlRender (HTMLString s) = s
htmlRender (HTMLPair a b) = (htmlRender a) `T.append` (htmlRender b)
htmlRender HTMLNothing = ""

tag :: Text -> Text -> [(Text, Text)] -> HTML
tag name contents attrs = HTMLString $ "<" <> name <> " " <> attrsS <> ">" <> contents <> "</" <> name <> ">"
  where attrsS = T.intercalate " " kevs
        kevs = [key <> "=" <> quot value | (key, value) <- attrs]
        quot s = "\"" <> s <> "\""
utag name = HTMLString $ "<" <> name <> "/>"

htmlList :: [HTML] -> HTML
htmlList htmls = mconcat htmls
col :: [HTML] -> HTML
col htmls = htmlList $ intersperse br htmls

br = utag "br"

link :: Text -> [Text] -> HTML
link text target = tag "a" text [("href", linkEncode target)]

linkEncode :: [Text] -> Text
linkEncode ss = "?q=" <> (T.pack $ ENC.encode $ show $ map T.unpack ss)

linkDecode :: Text -> [Text]
linkDecode s = read (ENC.decode $ T.unpack s)

instance Semigroup HTML where
  a <> b = HTMLPair a b

instance Monoid HTML where
  mempty = HTMLNothing

data WebResult = WROk HTML | WRRedirect String
  deriving Show

instance ToResponse WebResult where
  toResponse (WROk html) = toResponse ((htmlRender $ html) :: Text, ok200, M.fromList [("Content-type", ["text/html"])] :: HeaderMap)
  toResponse (WRRedirect url) = toResponse ("" :: Text, found302, M.fromList [("Location", [T.pack url])] :: HeaderMap)
