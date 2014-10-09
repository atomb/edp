module Term where

import Data.Maybe

import Utils

type Name = String

-- | A @Prop@ is an atom for purely propositional logic.
newtype Prop = Prop Name

-- | A @Pred@ is a potentially non-nullary first-order predicate
-- symbol.
data Pred
  = Exists Name
  | Relation Name [Name]

-- | A @Term@ is the top-level "paper" from in C. S. Peirce's
-- terminology, or anything inside a negation.
newtype Term a = Clauses [Clause a]
  deriving (Eq, Ord, Show)

-- | A @Clause@ is either an atom or some number of clauses inside a
-- negation.
data Clause a
  = Atom a
  | Not (Term a)
  deriving (Eq, Ord, Show)

-- | A path represents a series of nested indices.
--
--   * The empty path indicates the whole term in question.
--
--   * The singleton path represents the nth sub-clause of the current
--   term.
--
--   * A path of length greater than one represents a sub-term of a
--   negation, and is an error if the clause at the index given by its
--   head is an atom.
--
--   Note that paths can be used in two ways. When considering
--   insertion, the path indicates the @Term@ "container" into which
--   to insert the new element (so, either the empty list, indicating
--   the top level, or the path to a negation clause). When
--   considering deletion or retrieval, the path indicates a clause.
type Path = [Int]

-- | Insert a list of clauses inside the term reachable by the given
-- path (so the path should lead to a term).
insert :: Term a -> Path -> Term a -> Maybe (Term a)
insert (Clauses cs) [] (Clauses cs') = Just (Clauses (cs' ++ cs))
insert (Clauses cs) (n:_) _ | n >= length cs = Nothing
insert (Clauses cs) (n:ns) tins = do
  case cs !! n of
    Atom _ -> Nothing
    Not t -> do
      t' <- insert t ns tins
      cs' <- replace cs n (Not t')
      return (Clauses cs')

insertClause :: Term a -> Path -> Clause a -> Maybe (Term a)
insertClause t p c = insert t p (Clauses [c])

-- | Delete the clause reachable by the given path.
delete :: Term a -> Path -> Maybe (Term a)
delete _ [] = Nothing -- TODO: should this be Clauses []?
delete (Clauses cs) (n:_) | n >= length cs = Nothing
delete (Clauses cs) [n] = Just (Clauses (remove n cs))
delete (Clauses cs) (n:ns) =
  case cs !! n of
    Atom _ -> Nothing
    Not t -> do
      t' <- delete t ns
      cs' <- replace cs n (Not t')
      return (Clauses cs')

-- | Retrieve the clause reachable by the given path.
get :: Term a -> Path -> Maybe (Clause a)
get t [] = Nothing
get (Clauses cs) (n:_) | n >= length cs = Nothing
get (Clauses cs) [n] = Just (cs !! n)
get (Clauses cs) (n:ns) =
  case cs !! n of
    Atom _ -> Nothing -- Can't follow a path down from an atom
    Not t -> get t ns

-- | Determine whether the given path is valid within the given term.
validPath :: Term a -> Path -> Bool
validPath t p = isJust (get t p)

-- | Copy the clause reachable via the first path into the subterm
-- reachable via the second path.
copy :: Term a -> Path -> Path -> Maybe (Term a)
copy t p1 p2 = insertClause t p2 =<< get t p1

-- | Duplicate the subterm of the given term reachable via the given
-- path, parallel to the existing instance of the term.
duplicate :: Term a -> Path -> Maybe (Term a)
duplicate t p = copy t p (init p)

-- | Determine whether the clause is contained in the given term along
-- the given path.
{-
subtermAbove :: Path -> Clause a -> Term a -> Bool
subtermAbove p c (Clauses cs) =
  c `elem` cs ||
  case p of
    [] -> False
    i : is | i < length cs -> 
           | otherwise -> False
-}

-- TODO: should this always use p? Sometimes init p?
deleteAllowed :: Term a -> Path -> Bool
deleteAllowed t p =
  positive p || maybe False (\t' -> subtermAbove p t' t) (get t p)

-- TODO: should this always use p? Sometimes init p?
insertAllowed :: Term a -> Path -> Term a -> Bool
insertAllowed t' p t = negative p || subtermAbove p t' t

-- These take a path of the form passed to @insert@. So the empty path
-- is positive (unshaded).

positive, negative :: Path -> Bool
positive [] = True
positive (_:p) = negative p
negative [] = False
negative (_:p) = positive p

removeAllDoubleNot :: Term a -> Term a
removeAllDoubleNot (Clauses cs) = Clauses (concatMap unwrapClause (map go cs))
  where
    go (Not t) = (Not (removeAllDoubleNot t))
    go c@(Atom _) = c

unwrapClause :: Clause a -> [Clause a]
unwrapClause (Not (Clauses [Not (Clauses cs')])) = cs'
unwrapClause c = [c]

wrapClauses :: [Clause a] -> Clause a
wrapClauses cs = Not (Clauses [Not (Clauses cs)])

{-
explodeMap :: (Clause a -> [Clause a]) -> Term a -> Path -> Maybe (Term a)

gatherMap :: ([Clause a] -> Clause a) -> Term a -> Path -> Maybe (Term a)

removeDoubleNot :: Term a -> Path -> Maybe (Term a)

addDoubleNot :: Term a -> Path -> Maybe (Term a)
-}
