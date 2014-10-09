{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module ToHtml where

import           Control.Monad.Writer.Lazy
import           Data.CSS
import           Data.String
import           Data.Tree
import           Prelude hiding (div)
import           Text.Blaze.Html5 (Html, toHtml, div, (!))
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes (class_, href)
import qualified Text.Blaze.Html5.Attributes as A

import TreeTerm

posId :: Pos -> H.Attribute
posId = A.id . fromString . showPos

atomPosId :: AtomPos -> H.Attribute
atomPosId = A.id . fromString . showAtomPos

deleteURI :: Pos -> Html -> Html
deleteURI p b =
  H.a ! href (fromString ("/delete?pos=" ++ showPos p)) $ b

deleteAtomURI :: AtomPos -> Html -> Html
deleteAtomURI p b =
  H.a ! href (fromString ("/deleteAtom?pos=" ++ showAtomPos p)) $ b

copyURI :: Pos -> Pos -> Html -> Html
copyURI p p' b =
  H.a ! href (fromString ("/copy?src=" ++
                          showPos p ++
                          "&dst=" ++
                          showPos p')) $ b

addDoubleNeg :: Pos -> Html -> Html
addDoubleNeg p b =
  H.a ! href (fromString ("/addnotnot?pos=" ++ showPos p)) $ b

delDoubleNeg :: Pos -> Html -> Html
delDoubleNeg p b =
  H.a ! href (fromString ("/delnotnot?pos=" ++ showPos p)) $ b

atomDiv :: Bool -> AtomPos -> Html -> Html
atomDiv pol p h =
  div ! class_ "atom" ! atomPosId p $ h >> deleteAtomLink pol p

deleteLink :: Bool -> Pos -> Html
deleteLink False _ = ""
deleteLink True p = toHtml [ "[", deleteURI p "remove", "]" ]

deleteAtomLink :: Bool -> AtomPos -> Html
deleteAtomLink True _ = ""
deleteAtomLink False p = toHtml [ "[", deleteAtomURI p "remove", "]" ]

notDiv :: Bool -> Bool -> Pos -> Html -> Html
notDiv isDonut pol p h = div ! class_ cls ! posId p $
                         deleteLink isDonut p >> h
  where
    cls = fromString $ unwords [polText, donutText, "not"]
    polText = if pol then "unshaded" else "shaded"
    donutText = if isDonut then "donut" else ""

renderTerm :: (a -> Html) -> Bool -> Pos -> Term a -> Html
renderTerm renderAtom pol p tt@(Node as ts) =
  toHtml $ map doAtom (zip [0..] as) ++ map doNot (zip [0..] ts)
    where
      doAtom (n, a) = atomDiv pol (AtomPos p n) (renderAtom a)
      doNot (n, t) = let p' = posExtend p n in
        notDiv (isDoubleNeg tt n) pol p' (renderTerm renderAtom (not pol) p' t)

renderTopTerm :: (a -> Html) -> Term a -> Html
renderTopTerm renderAtom t =
  div ! class_ "top" $ renderTerm renderAtom False (Pos []) t

renderPage :: (H.ToMarkup a) => Term a -> Html
renderPage t = H.docTypeHtml $ do
   H.head $ do
    H.title "Formula rendering"
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! href "edp.css"
   H.body $ renderTopTerm toHtml t

stylesheet :: Writer CSS ()
stylesheet = do
  onAll . select [".top"] $ do
    setProp "width" ("200px" :: PropValue)
  onAll . select [".not"] $ do
    setProp "padding" ("10px" :: PropValue)
    setProp "width" ("auto" :: PropValue)
    setProp "margin" ("4px" :: PropValue)
  onAll . select [".shaded"] $ do
    setProp "background-color" ("gray" :: PropValue)
  onAll . select [".unshaded"] $ do
    setProp "background-color" ("white" :: PropValue)
  onAll . select [".donut"] $ do
    setProp "border" ("1px dashed black" :: PropValue)
  onAll . select [".atom"] $ do
    setProp "cursor" ("default" :: PropValue)
  onAll . select [".atom:hover"] $ do
    setProp "color" ("red" :: PropValue)

{-
hoverScript :: Html
hoverScript = H.script . toHtml . unlines $
  [ "$(\"div\").on(\"mouseover\",function(e){"
  , "  $(this).css(\"color\",\"red\");"
  , "  e.stopPropagation();"
  , "});"
  , "$(\"div\").on(\"mouseout\",function(e){"
  , "  $(this).css(\"color\",\"black\");"
  , "});"
  ]
-}
