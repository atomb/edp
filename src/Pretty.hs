{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import Data.List
import Data.Tree
import Text.PrettyPrint.ANSI.Leijen

import TreeTerm

-- | Pretty-print a term with spaces between atoms and parentheses for
-- negation.
prettyTerm :: (Show a) => Term a -> String
prettyTerm (Node as ts) =
  unwords (map show as ++ map showTerm ts)
    where showTerm t = "(" ++ prettyTerm t ++ ")"

-- | Pretty-print a term with conjunction symbols between atoms and
-- tildes for negation.
prettyTermLogic :: (Show a) => Term a -> String
prettyTermLogic (Node as ts) =
  unwords (intersperse "/\\" ((map show as) ++ map showTerm ts))
    where showTerm t = "~(" ++ prettyTermLogic t ++ ")"

-- | Pretty-print a term with spaces between atoms and parentheses for
-- negation, coloring negative regions with a dark background color.
prettyTermColor :: (Show a) => Bool -> Term a -> Doc
prettyTermColor pol (Node as ts) =
  col (hcat (map ((space <>) . text . show) as)) <> hcat (map showTerm ts)
    where showTerm t = col space <> (cparens (prettyTermColor (not pol) t))
          col = if pol then id else ondullblack
          ncol = if pol then ondullblack else id
          cparens d = parens (d <> ncol space)

colorTerm :: (Show a) => Term a -> IO ()
colorTerm = putDoc . (<> linebreak) . prettyTermColor True
