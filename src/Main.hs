{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Blaze.ByteString.Builder
import           Control.Monad.Writer.Lazy
import           Data.CSS
import           Snap.Core (Snap)
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html.Renderer.Utf8 as H

import Examples
import ToHtml
import TreeTerm

main :: IO ()
main = Snap.quickHttpServe $ Snap.route $
  [ ("splendid",      serveTerm splendid)
  , ("frege1",        serveTerm frege1)
  , ("contradiction", serveTerm contradiction)
  , ("false",         serveTerm (false :: Term String))
  , ("true",          serveTerm (true :: Term String))
  , ("edp.css",       serveCSS stylesheet)
  ]

-- | Render an HTML version of a term and send it to the client. The
-- term will be negated, so the user's goal is to reduce it to False,
-- thereby proving its validity by contradiction.
serveTerm :: (H.ToMarkup a) => Term a -> Snap ()
serveTerm = Snap.writeLBS . H.renderHtml . renderPage . tnot

serveCSS :: Writer CSS () -> Snap ()
serveCSS = Snap.writeLBS . toLazyByteString . renderCSS
