{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Core
  ( WM, WMState(..), WMEnv(..), WindowSet, runWM
  , LayoutClass(..)
  , Layout(..)
  , Message, SomeMessage(..), fromMessage
  , Query(..), ManageHook, runQuery, applyManageHook
  ) where

import qualified StackSet as SS
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Monoid (Endo(..))
import Data.Typeable (Typeable, cast)
import Wayfire (Geometry, WayfireWindow)
import Network.Socket (Socket)
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative (liftA2)
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader)
import Control.Monad.State  (StateT, runStateT, MonadState, modify)

type WindowSet = SS.StackSet String Layout Int Int

data WMState = WMState
  { stackSet          :: WindowSet
  , geomMap           :: Map Int Geometry
  , outputMap         :: Map Int Geometry
  , compositorManaged :: Set Int
  }

data WMEnv = WMEnv
  { wmSocket   :: Socket
  , bindingMap :: Map Int (WM ())
  , layoutHook :: Layout
  , manageHook :: ManageHook
  }

newtype WM a = WM (ReaderT WMEnv (StateT WMState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO,
            MonadState WMState, MonadReader WMEnv)

runWM :: WMEnv -> WMState -> WM a -> IO (a, WMState)
runWM env st (WM action) = runStateT (runReaderT action env) st

class Typeable a => Message a

data SomeMessage = forall a. Message a => SomeMessage a

fromMessage :: Message m => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m

class LayoutClass layout where
    runLayout :: layout
              -> Geometry
              -> SS.Stack Int
              -> Map Int Geometry
              -> WM ([(Int, Geometry)], Maybe Layout)
    runLayout l screen stack gm = return (pureLayout l screen stack gm, Nothing)

    pureLayout :: layout
               -> Geometry
               -> SS.Stack Int
               -> Map Int Geometry
               -> [(Int, Geometry)]
    pureLayout _ _ _ _ = []

    handleMessage :: layout -> SomeMessage -> WM (Maybe Layout)
    handleMessage _ _ = return Nothing

data Layout = forall l. (LayoutClass l, Show l) => Layout l

instance Show Layout where
    show (Layout l) = show l

instance LayoutClass Layout where
    runLayout     (Layout l) = runLayout l
    pureLayout    (Layout l) = pureLayout l
    handleMessage (Layout l) = handleMessage l

newtype Query a = Query (ReaderT WayfireWindow WM a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader WayfireWindow)

instance Semigroup a => Semigroup (Query a) where
    f <> g = liftA2 (<>) f g

instance Monoid a => Monoid (Query a) where
    mempty = pure mempty

type ManageHook = Query (Endo WMState)

runQuery :: Query a -> WayfireWindow -> WM a
runQuery (Query q) = runReaderT q

applyManageHook :: ManageHook -> WayfireWindow -> WM ()
applyManageHook mh w = runQuery mh w >>= \e -> modify (appEndo e)
