{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Properties where

import Control.Applicative
import Data.Maybe
import Data.Tree
import Test.QuickCheck

import TreeTerm

prop_hasAddDoubleNeg :: Term Name -> Bool
prop_hasAddDoubleNeg = hasDoubleNeg . addDoubleNeg

prop_parseShowPos :: Pos -> Bool
prop_parseShowPos p = parsePos (showPos p) == Just p

prop_parseShowAtomPos :: AtomPos -> Bool
prop_parseShowAtomPos p = parseAtomPos (showAtomPos p) == Just p

prop_allPosDelete :: PosTerm Name -> Bool
-- It's ok that we can't delete the entire term.
prop_allPosDelete (PosTerm (Pos []) _) = True
prop_allPosDelete (PosTerm p t) = isJust (deleteSubterm p t)

prop_allPosGet :: PosTerm Name -> Bool
prop_allPosGet (PosTerm p t) = isJust (getSubterm p t)

prop_allPosDup :: PosTerm Name -> Bool
-- It's ok that we can't duplicate the entire term.
prop_allPosDup (PosTerm (Pos []) _) = True
prop_allPosDup (PosTerm p t) = isJust (dupSubterm p t)

prop_allAtomPosDelete :: AtomPosTerm Name -> Bool
prop_allAtomPosDelete (AtomPosTerm p t) = isJust (deleteAtom p t)

prop_allAtomPosGet :: AtomPosTerm Name -> Bool
prop_allAtomPosGet (AtomPosTerm p t) = isJust (getAtom p t)

prop_allAtomPosDup :: AtomPosTerm Name -> Bool
prop_allAtomPosDup (AtomPosTerm p t) = isJust (dupAtom p t)

data Name = Name String
  deriving (Eq, Ord, Show)

data PosTerm a = PosTerm Pos (Term a)
  deriving (Show)

data AtomPosTerm a = AtomPosTerm AtomPos (Term a)
  deriving (Show)

allPositions :: Term a -> [Pos]
allPositions (Node _ ts) = Pos [] : concatMap subPositions (zip [0..] ts)
  where
    prependPos i (Pos is) = Pos (i:is)
    subPositions (n, t) = map (prependPos n) (allPositions t)

allAtomPositions :: Term a -> [AtomPos]
allAtomPositions t = go (Pos []) t
  where
    go p (Node as ts) = map (AtomPos p . fst) (zip [0..] as) ++
                        concatMap subPositions (zip [0..] ts)
      where subPositions (n, t') = go (posExtend p n) t'

instance Arbitrary Pos where
  arbitrary = Pos <$> arbitrary

instance Arbitrary AtomPos where
  arbitrary = AtomPos <$> arbitrary <*> arbitrary

instance Arbitrary Name where
  arbitrary = elements (map Name ["a", "b", "c", "d", "e"])

instance (Arbitrary a) => Arbitrary (Term a) where
  arbitrary = sized arbTerm
    where
      arbTerm n = do
        natoms <- elements [0..n]
        nsubs <- elements [0..n]
        atoms <- vectorOf natoms arbitrary
        subs <- vectorOf nsubs (arbTerm (n `div` 2))
        return (Node atoms subs)

instance (Arbitrary a) => Arbitrary (PosTerm a) where
  arbitrary = sized arbTermPos
    where
      arbTermPos n = do
        t <- resize n arbitrary
        p <- elements (allPositions t)
        return (PosTerm p t)


instance (Arbitrary a) => Arbitrary (AtomPosTerm a) where
  arbitrary = sized arbTermAtomPos
    where
      arbTermAtomPos n = do
        t <- resize n arbitrary
        let ps = allAtomPositions t
        t' <- if null ps then atom <$> arbitrary else return t
        p <- elements (allAtomPositions t')
        return (AtomPosTerm p t')

testAll :: IO ()
testAll = mapM_ (quickCheckWith (stdArgs { maxSize = 30 }))
          [ label "hasAddDoubleNeg"  prop_hasAddDoubleNeg
          , label "parseShowPos"     prop_parseShowPos
          , label "parseShowAtomPos" prop_parseShowAtomPos
          , label "allPosDelete"     prop_allPosDelete
          , label "allPosGet"        prop_allPosGet
          , label "allPosDup"        prop_allPosDup
          , label "allAtomPosDelete" prop_allAtomPosDelete
          , label "allAtomPosGet"    prop_allAtomPosGet
          , label "allAtomPosDup"    prop_allAtomPosDup
          ]
