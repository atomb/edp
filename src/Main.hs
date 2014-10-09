{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Blaze.ByteString.Builder
import           Control.Monad.Writer.Lazy
import           Data.ByteString.Char8 (unpack)
import           Data.CSS
import           Data.IORef ( IORef, modifyIORef', newIORef, readIORef
                            , writeIORef)
import           Snap.Core (Snap)
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html.Renderer.Utf8 as H

import Examples
import ToHtml
import TreeTerm

main :: IO ()
main = do
  s <- newSession
  Snap.quickHttpServe $ Snap.route $
    [ ("splendid",        serveTerm s splendid)
    , ("frege1",          serveTerm s frege1)
    , ("contradiction",   serveTerm s contradiction)
    , ("false",           serveTerm s (false :: Term String))
    , ("true",            serveTerm s (true :: Term String))
    , ("deleteTerm",      doDeleteTerm s)
    , ("deleteAtom",      doDeleteAtom s)
    , ("delNotNot",       doDeleteDoubleNeg s)
    , ("edp.css",         serveCSS stylesheet)
    ]

-- | Render an HTML version of a term and send it to the client. The
-- term will be negated, so the user's goal is to reduce it to False,
-- thereby proving its validity by contradiction.
serveTerm :: (H.ToMarkup a, Ord a, Show a) => Session a -> Term a -> Snap ()
serveTerm s t = do
  let t' = tnot t
  -- TODO: this is not thread safe, so we'd better not have multiple clients
  liftIO $ do
    writeIORef (originalTerm s) t'
    writeIORef (actions s) []
    writeIORef (currentTerm s) t'
  serveCurrentTerm s

serveCurrentTerm :: (H.ToMarkup a, Ord a, Show a) => Session a -> Snap ()
serveCurrentTerm s = do
  t <- liftIO $ readIORef (currentTerm s)
  Snap.writeLBS . H.renderHtml . renderPage $ t

serveCSS :: Writer CSS () -> Snap ()
serveCSS = Snap.writeLBS . toLazyByteString . renderCSS

data Action
  = Delete Pos
  | DeleteAtom AtomPos
  | DeleteDoubleNeg Pos
  | CopyTerm Pos Pos
  | CopyAtom AtomPos Pos

prettyAction :: Action -> String
prettyAction a =
  case a of
    Delete p -> "delete " ++ showPos p
    DeleteAtom p -> "delete atom " ++ showAtomPos p
    DeleteDoubleNeg p ->
      "delete double negation at " ++ showPos p
    CopyTerm p p' ->
      "copy term at " ++ showPos p ++ " to " ++ showPos p'
    CopyAtom p p' ->
      "copy atom at " ++ showAtomPos p ++ " to " ++ showPos p'

data Session a = Session
  { originalTerm :: IORef (Term a)
  , actions      :: IORef [Action]
  , currentTerm  :: IORef (Term a)
  }

newSession :: IO (Session a)
newSession = do
  originalTerm <- newIORef true
  actions      <- newIORef []
  currentTerm  <- newIORef true
  return Session { .. }

runAction :: (H.ToMarkup a, Eq a, Ord a, Show a) => Session a -> Action -> Snap ()
runAction s a = do
  t <- liftIO $ readIORef (currentTerm s)
  case applyAction a t of
    Just t' -> liftIO $ do
        writeIORef (currentTerm s) t'
        modifyIORef' (actions s) (a:)
        putStrLn (prettyAction a)
    Nothing -> liftIO $
      putStrLn "Action returned Nothing"
  serveCurrentTerm s

applyAction :: (Eq a) => Action -> Term a -> Maybe (Term a)
applyAction a t =
  case a of
    Delete p           -> go (deleteAllowed p t)            (deleteSubterm p)
    DeleteAtom p       -> go (deleteAtomAllowed p t)        (deleteAtom p)
    DeleteDoubleNeg p  -> go True                           (deleteDoubleNeg p)
    CopyTerm p p'      -> go (copyAllowed p p')             (copySubterm p p')
    CopyAtom p p'      -> go (copyAtomAllowed p p')         (copyAtom p p')
  where go g f = guard g >> f t

doPosMethod :: (H.ToMarkup a, Ord a, Show a) =>
               (String -> Maybe p) -> (Session a -> p -> Snap ()) -> Session a
            -> Snap ()
doPosMethod rdr hndlr s = do
  mpos <- Snap.getParam "pos"
  case mpos of
    Nothing -> do
      liftIO $ putStrLn "Method missing 'pos' parameter."
      serveCurrentTerm s
    Just bs -> do
      case rdr (unpack bs) of
        Nothing -> do
          liftIO $ putStrLn "Method has malformed 'pos' parameter."
          serveCurrentTerm s
        Just p -> hndlr s p

doDeleteTerm :: (H.ToMarkup a, Eq a, Ord a, Show a) => Session a -> Snap ()
doDeleteTerm = doPosMethod parsePos (\s p -> runAction s (Delete p))

doDeleteAtom :: (H.ToMarkup a, Eq a, Ord a, Show a) => Session a -> Snap ()
doDeleteAtom = doPosMethod parseAtomPos (\s p -> runAction s (DeleteAtom p))

doDeleteDoubleNeg :: (H.ToMarkup a, Eq a, Ord a, Show a) => Session a -> Snap ()
doDeleteDoubleNeg =
  doPosMethod parsePos (\s p -> runAction s (DeleteDoubleNeg p))
