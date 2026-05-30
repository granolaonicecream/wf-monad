{-# LANGUAGE OverloadedRecordDot #-}
module ManageHook
  ( ManageHook
  , Query
  , (-->)
  , (=?)
  , composeAll
  , idHook
  , ManageHook.title
  , ManageHook.appId
  , wfRole
  , doShift
  , doFloat
  , doIgnore
  , ask
  ) where

import Core (Query(..), ManageHook, WMState(..))
import Wayfire (WayfireWindow(..))
import qualified StackSet as SS
import Data.Monoid (Endo(..))
import qualified Data.Set as Set
import Control.Monad.Reader (ask, asks)

-- ---------------------------------------------------------------------------
-- Property queries
-- ---------------------------------------------------------------------------

title :: Query String
title = asks (.title)

appId :: Query String
appId = asks (.appId)

wfRole :: Query String
wfRole = asks (.role)

-- ---------------------------------------------------------------------------
-- Combinators
-- ---------------------------------------------------------------------------

infix 1 =?
(=?) :: Eq a => Query a -> a -> Query Bool
q =? x = fmap (== x) q

infixr 0 -->
(-->) :: Monoid m => Query Bool -> Query m -> Query m
p --> f = p >>= \b -> if b then f else return mempty

composeAll :: [ManageHook] -> ManageHook
composeAll = mconcat

idHook :: ManageHook
idHook = return mempty

-- ---------------------------------------------------------------------------
-- Actions
-- ---------------------------------------------------------------------------

doShift :: String -> ManageHook
doShift wsTag = do
    w <- ask
    return $ Endo $ \st -> st
        { stackSet = SS.insertInto wsTag w.id (SS.delete w.id (stackSet st)) }

doFloat :: ManageHook
doFloat = do
    w <- ask
    return $ Endo $ \st -> st
        { stackSet = (stackSet st) { SS.floating = Set.insert w.id (SS.floating (stackSet st)) } }

doIgnore :: ManageHook
doIgnore = do
    w <- ask
    return $ Endo $ \st -> st
        { stackSet = SS.delete w.id (stackSet st) }
