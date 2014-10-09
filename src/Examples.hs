module Examples (contradiction, frege1, splendid, showTerm) where

import Text.Blaze.Html5 (ToMarkup)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import TreeTerm
import ToHtml

a, b, p, q, r, s :: Term String
a = atom "A"
b = atom "B"
p = atom "P"
q = atom "Q"
r = atom "R"
s = atom "S"

contradiction :: Term String
contradiction = tand a (tnot a)

frege1 :: Term String
frege1 = timp a (timp b a)

splendid :: Term String
splendid = ((p `timp` r) `tand` (q `timp` s)) `timp`
           ((p `tand` q) `timp` (r `tand` s))

showTerm :: (ToMarkup a, Ord a, Show a) => Term a -> IO ()
showTerm = putStrLn . renderHtml . renderPage

