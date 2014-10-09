{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module ToHtml where

import           Control.Monad.Writer.Lazy
import           Data.CSS
import           Data.String
import qualified Data.Set as Set
import           Data.Set (Set)
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

binopLink :: String -> (p -> String) -> p -> p -> Html -> Html
binopLink method pShow p p' b =
  H.a ! href (fromString ("/" ++ method ++ "?outer=" ++
                          pShow p ++
                          "&inner=" ++
                          pShow p')) $ b

unopLink :: String -> (p -> String) -> p -> Html -> Html
unopLink method pShow p b =
  H.a ! href (fromString ("/" ++ method ++ "?pos=" ++ pShow p)) $ b

delTermLink :: Pos -> Html -> Html
delTermLink = unopLink "deleteTerm" showPos

delAtomLink :: AtomPos -> Html -> Html
delAtomLink = unopLink "deleteAtom" showAtomPos

addDoubleNegLink :: Pos -> Html -> Html
addDoubleNegLink = unopLink "addNotNot" showPos

delDoubleNegLink :: Pos -> Html -> Html
delDoubleNegLink = unopLink "delNotNot" showPos

copyTermLink :: Pos -> Pos -> Html -> Html
copyTermLink = binopLink "copyTerm" showPos

copyAtomLink :: AtomPos -> AtomPos -> Html -> Html
copyAtomLink = binopLink "copyAtom" showAtomPos

atomDiv :: Bool -> Bool -> AtomPos -> Html -> Html
atomDiv pol seen p h =
  div ! class_ "atom" ! atomPosId p $
    h >> delAtomText (pol && not seen) p

delTermText :: Bool -> Pos -> Html
delTermText False _ = ""
delTermText True p = toHtml [ "[", delTermLink p "remove", "]" ]

delNotNotText :: Bool -> Pos -> Html
delNotNotText False _ = ""
delNotNotText True p = toHtml [ "[", delDoubleNegLink p "remove", "]" ]

delAtomText :: Bool -> AtomPos -> Html
delAtomText True _ = ""
delAtomText False p = toHtml [ "[", delAtomLink p "remove", "]" ]

notDiv :: Bool -> Bool -> Bool -> Bool -> Pos -> Html -> Html
notDiv isDonut pol seen emp p h =
  div ! class_ cls ! posId p $
    h >> delNotNotText isDonut p >> delTermText seen p >> emptyText
      where
        cls = fromString $ unwords [polText, donutText, "not"]
        polText = if pol then "unshaded" else "shaded"
        donutText = if isDonut then "donut" else ""
        emptyText = if emp && not pol then "Done!" else ""

renderTerm :: (Eq a, Ord a, Show a) =>
              (a -> Html) -> Set a -> Set (Term a) -> Bool -> Pos -> Term a -> Html
renderTerm renderAtom atoms tms pol p tt@(Node as ts) =
  toHtml $ map doAtom (zip [0..] as) ++ map doNot (zip [0..] ts)
    where
      doAtom (n, a) = atomDiv pol (Set.member a atoms) (AtomPos p n) (renderAtom a)
      doNot (n, t) = let p' = posExtend p n in
        notDiv (isDoubleNeg tt n) pol (Set.member t tms) (t == true) p' $
          renderTerm renderAtom atoms' tms' (not pol) p' t
      atoms' = Set.union atoms (Set.fromList as)
      tms' = Set.union tms (Set.fromList ts)

renderTopTerm :: (Ord a, Show a) => (a -> Html) -> Term a -> Html
renderTopTerm renderAtom t =
  div ! class_ "top" $
    renderTerm renderAtom Set.empty Set.empty False (Pos []) t

renderPage :: (H.ToMarkup a, Ord a, Show a) => Term a -> Html
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
