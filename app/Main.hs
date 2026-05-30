{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Wayfire
import Core
import Layout (Tall(..), Resize(..), IncMasterN(..), TwoPane(..), NextLayout(..), FirstLayout(..), (|||))
import ManageHook
import Network.Socket
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Environment (getEnv)
import qualified Network.Socket.ByteString as SB
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import StackSet
import qualified StackSet as SS
import Control.Monad (foldM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.State  (get, modify)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.List (sortOn)
import Data.Maybe (fromMaybe)

-- ============================================================
-- Config
-- ============================================================

data Config = Config
  { workspaces :: [String]
  , layoutHook :: Layout
  , manageHook :: ManageHook
  , keys       :: [(String, WM ())]
  }

-- ============================================================
-- User configuration
-- ============================================================

myConfig :: Config
myConfig = Config
    { workspaces = myWorkspaces
    , layoutHook = myLayout
    , manageHook = myManageHook
    , keys       = myKeys
    }

myWorkspaces :: [String]
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myLayout :: Layout
myLayout = Layout (Tall { tallNMaster = 1, tallDelta = 3/100, tallRatio = 1/2 })
        ||| Layout (TwoPane { twoPaneDelta = 3/100, twoPaneRatio = 1/2 })

myManageHook :: ManageHook
myManageHook = idHook

myKeys :: [(String, WM ())]
myKeys =
    [ ("<super> KEY_J",               windows (modifyFocus focusUp))
    , ("<super> KEY_K",               windows (modifyFocus focusDown))
    , ("<super> <shift> KEY_J",       windows (modifyFocus SS.swapUp))
    , ("<super> <shift> KEY_K",       windows (modifyFocus SS.swapDown))
    , ("<super> KEY_H",               sendMessage Shrink)
    , ("<super> KEY_L",               sendMessage Expand)
    , ("<super> KEY_COMMA",           sendMessage (IncMasterN 1))
    , ("<super> KEY_DOT",             sendMessage (IncMasterN (-1)))
    , ("<super> KEY_T",               toggleUntiled)
    , ("<super> KEY_SPACE",           sendMessage NextLayout)
    , ("<super> <shift> KEY_SPACE",   sendMessage FirstLayout)
    ] ++
    [ ("<super> KEY_" ++ t,           windows (SS.greedyView t))   | t <- myWorkspaces ] ++
    [ ("<super> <shift> KEY_" ++ t,   windows (sendToWorkspace t)) | t <- myWorkspaces ]

-- ============================================================
-- Helpers
-- ============================================================

type StackTransform i l a sid = SS.StackSet i l a sid -> SS.StackSet i l a sid

workspaceStride :: Int
workspaceStride = 100000

shiftedGeom :: Geometry -> Geometry
shiftedGeom g = Geometry { x = g.x + workspaceStride, y = g.y, width = g.width, height = g.height }

onVisibleWorkspace :: Int -> WindowSet -> Bool
onVisibleWorkspace wid ss =
  let inStack = maybe False (elem wid . SS.integrate)
  in inStack ss.current.workspace.stack
  || any (inStack . SS.stack . SS.workspace) ss.visible

windowInWorkspace :: Int -> String -> WindowSet -> Bool
windowInWorkspace wid wsTag ss =
  let allWs  = SS.workspace (SS.current ss)
             : map SS.workspace (SS.visible ss)
             ++ SS.hidden ss
      inStk  = maybe False (elem wid . SS.integrate) . SS.stack
  in any (\ws -> SS.tag ws == wsTag && inStk ws) allWs

outputToWorkspace :: WindowSet -> Map Int String
outputToWorkspace ss = Map.fromList $
  (ss.current.screenId, ss.current.workspace.tag) :
  map (\sc -> (sc.screenId, sc.workspace.tag)) ss.visible

-- ============================================================
-- WM operations
-- ============================================================

modifyFocus :: (SS.Stack Int -> SS.Stack Int) -> WindowSet -> WindowSet
modifyFocus f ss = ss { SS.current = ss.current
  { SS.workspace = ss.current.workspace
      { SS.stack = fmap f ss.current.workspace.stack }}}

sendToWorkspace :: String -> WindowSet -> WindowSet
sendToWorkspace wsTag ss
  | wsTag == ss.current.workspace.tag = ss
  | otherwise = case ss.current.workspace.stack of
      Nothing  -> ss
      Just stk -> SS.insertInto wsTag stk.focus (SS.delete stk.focus ss)

isUntiled :: WMState -> Int -> Bool
isUntiled st wid = Set.member wid (compositorManaged st)
                || Set.member wid (SS.floating (stackSet st))

updateUntiled :: WayfireWindow -> WMState -> WMState
updateUntiled w st = st
  { compositorManaged = (if w.fullscreen || w.minimized then Set.insert else Set.delete)
                        w.id (compositorManaged st) }

toggleUntiled :: WM ()
toggleUntiled = do
  st <- get
  case st.stackSet.current.workspace.stack of
    Nothing  -> return ()
    Just stk -> do
      let wid    = stk.focus
          toggle = if Set.member wid (SS.floating (stackSet st)) then Set.delete else Set.insert
      modify (\s -> s { stackSet = (stackSet s) { SS.floating = toggle wid (SS.floating (stackSet s)) } })
      sync st

windows :: (WindowSet -> WindowSet) -> WM ()
windows f = do
  old <- get
  modify (\st -> st { stackSet = f (stackSet st) })
  sync old

setCurrentLayout :: Layout -> WindowSet -> WindowSet
setCurrentLayout l ss = ss { SS.current = (SS.current ss)
  { SS.workspace = (SS.workspace (SS.current ss)) { SS.layout = l } } }

sendMessage :: Message a => a -> WM ()
sendMessage m = do
  st <- get
  let curLayout = SS.layout (SS.workspace (SS.current (stackSet st)))
  mNew <- handleMessage curLayout (SomeMessage m)
  case mNew of
    Nothing        -> return ()
    Just newLayout -> windows (setCurrentLayout newLayout)

focusUp :: SS.Stack Int -> SS.Stack Int
focusUp (SS.Stack f []      []) = SS.Stack f [] []
focusUp (SS.Stack f []  (d:ds)) = SS.Stack (last (d:ds)) (tail (reverse (d:ds)) ++ [f]) []
focusUp (SS.Stack f (u:us)   d) = SS.Stack u us (f:d)

focusDown :: SS.Stack Int -> SS.Stack Int
focusDown (SS.Stack f []     []) = SS.Stack f [] []
focusDown (SS.Stack f (u:us) []) = SS.Stack (last (u:us)) [] (tail (reverse (u:us)) ++ [f])
focusDown (SS.Stack f u  (d:ds)) = SS.Stack d (f:u) ds

-- ============================================================
-- Sync
-- ============================================================

data WindowLocation = VisibleOn Int | Offscreen deriving (Eq)

windowLocation :: WindowSet -> Int -> WindowLocation
windowLocation ss wid =
  let inScreen sc = maybe False (elem wid . SS.integrate) (SS.stack (SS.workspace sc))
  in if inScreen (SS.current ss)
     then VisibleOn (SS.screenId (SS.current ss))
     else case filter inScreen (SS.visible ss) of
       (sc:_) -> VisibleOn (SS.screenId sc)
       []     -> Offscreen

allWindows :: WindowSet -> [Int]
allWindows ss =
  concatMap (maybe [] SS.integrate . SS.stack . SS.workspace) (SS.current ss : SS.visible ss)
  ++ concatMap (maybe [] SS.integrate . SS.stack) (SS.hidden ss)

sync :: WMState -> WM ()
sync old = do
  new  <- get
  sock <- asks wmSocket
  let screens = SS.current (stackSet new) : SS.visible (stackSet new)
  layoutLists <- mapM (layoutForScreen new) screens
  let layoutPos = Map.fromList (concat layoutLists)
  liftIO $ do
    forM_ (allWindows (stackSet new)) $ \wid -> do
      let oldLoc = windowLocation (stackSet old) wid
          newLoc = windowLocation (stackSet new) wid
      case newLoc of
        Offscreen   -> when (oldLoc /= newLoc) $
                         case Map.lookup wid (geomMap old) of
                           Just g  -> SB.sendAll sock (configureView wid (shiftedGeom g))
                           Nothing -> return ()
        VisibleOn s -> when (not (isUntiled new wid)) $
                         case Map.lookup wid layoutPos of
                           Just g  -> SB.sendAll sock (moveViewToOutput wid g s)
                           Nothing -> return ()
    case (stackSet new).current.workspace.stack of
      Just stk -> when (not (isUntiled new stk.focus)) $
                    SB.sendAll sock (focusView stk.focus)
      Nothing  -> return ()
  where
    layoutForScreen new sc =
      case (SS.filterStack (\w -> not (isUntiled new w)) (SS.stack (SS.workspace sc)),
            Map.lookup (SS.screenId sc) (outputMap new)) of
        (Just stk, Just screenGeom) ->
          fst <$> runLayout (SS.layout (SS.workspace sc)) screenGeom stk (geomMap new)
        _ -> return []

-- ============================================================
-- Event loop
-- ============================================================

dispatch :: WayfireMessage -> WM ()
dispatch msg = case msg of
  Heartbeat txt -> liftIO $ putStrLn ("[*] " ++ txt)

  Snapshot _ -> return ()

  OutputFocus oid -> do
    modify (\st -> st { stackSet = SS.focusScreen oid (stackSet st) })
    liftIO $ putStrLn $ "[F] Output focus -> " ++ show oid

  Signal sig
    | sig.view.role /= "toplevel" -> return ()
    | otherwise -> do
        old <- get
        modify (updateUntiled sig.view)
        case sig.event of
          "view-mapped" -> do
            st <- get
            let wid   = sig.view.id
                owMap = outputToWorkspace (stackSet st)
                wsTag = fromMaybe st.stackSet.current.workspace.tag
                                  (Map.lookup sig.view.outputId owMap)
            modify $ \s -> WMState
              (SS.insertInto wsTag wid (SS.delete wid (stackSet s)))
              (Map.insert wid sig.view.geometry (geomMap s))
              (outputMap s)
              (compositorManaged s)
            env <- ask
            applyManageHook env.manageHook sig.view
            sync old
            liftIO $ putStrLn $ "[+] Window Opened -> " ++ show wid ++ " (" ++ sig.view.title ++ ")"
          "view-unmapped" -> do
            modify (\s -> s { stackSet          = SS.delete sig.view.id (stackSet s)
                            , compositorManaged = Set.delete sig.view.id (compositorManaged s) })
            sync old
            liftIO $ putStrLn $ "[-] Window Closed -> " ++ show sig.view.id
          "view-focused" -> do
            modify (\s -> s { stackSet = SS.focusWindow sig.view.id (stackSet s) })
            liftIO $ putStrLn $ "[~] Focus -> " ++ show sig.view.id ++ " (" ++ sig.view.title ++ ")"
          "view-set-output" -> do
            st <- get
            let wid   = sig.view.id
                owMap = outputToWorkspace (stackSet st)
                wsTag = fromMaybe st.stackSet.current.workspace.tag
                                  (Map.lookup sig.view.outputId owMap)
                newSS = if windowInWorkspace wid wsTag (stackSet st)
                        then stackSet st
                        else SS.insertInto wsTag wid (SS.delete wid (stackSet st))
            modify $ \s -> WMState newSS
              (Map.insert wid sig.view.geometry (geomMap s))
              (outputMap s)
              (compositorManaged s)
            liftIO $ putStrLn $ "[O] Window " ++ show wid ++ " -> output " ++ show sig.view.outputId
          "view-minimized" -> sync old
          "view-geometry-changed" -> do
            st <- get
            when (onVisibleWorkspace sig.view.id (stackSet st)) $
              modify (\s -> s { geomMap = Map.insert sig.view.id sig.view.geometry (geomMap s) })
          _ -> return ()

  CommandBinding bid -> do
    bm <- asks bindingMap
    case Map.lookup bid bm of
      Nothing     -> liftIO $ putStrLn ("Unknown binding-id: " ++ show bid)
      Just action -> action

runSocketEngine :: WMEnv -> WMState -> B.ByteString -> IO ()
runSocketEngine env st buf = do
  result <- readNextFrame env.wmSocket buf
  case result of
    Nothing -> putStrLn "Wayfire socket closed."
    Just (payload, rest) -> do
      putStrLn $ "Payload: " ++ BC.unpack payload
      st' <- case parseIncomingMessage payload of
        Left err  -> putStrLn ("Parse error: " ++ err) >> return st
        Right msg -> snd <$> runWM env st (dispatch msg)
      runSocketEngine env st' rest

-- ============================================================
-- Initialization
-- ============================================================

synchronizeState :: [WayfireWindow] -> WMState -> WMState
synchronizeState ws st =
  let managed    = filter (\w -> w.mapped && w.role /= "desktop-environment") ws
      owMap      = outputToWorkspace (stackSet st)
      curTag     = st.stackSet.current.workspace.tag
      newGeomMap = foldr (\w m -> Map.insert w.id w.geometry m) (geomMap st) managed
      newSS      = foldr (\w ss -> SS.insertInto (fromMaybe curTag (Map.lookup w.outputId owMap)) w.id ss)
                         (stackSet st) managed
      newCM      = foldr (\w -> if w.fullscreen then Set.insert w.id else Prelude.id) (compositorManaged st) managed
  in WMState newSS newGeomMap (outputMap st) newCM

registerBindings :: Socket -> B.ByteString -> [(String, WM ())] -> IO (Map Int (WM ()), B.ByteString)
registerBindings sock buf0 = foldM step (Map.empty, buf0)
  where
    step (acc, buf) (combo, action) = do
      SB.sendAll sock (registerBinding combo)
      (resp, buf') <- readOneMsg sock buf
      case extractBindingId resp of
        Nothing  -> putStrLn ("Warning: no binding-id for " ++ combo) >> return (acc, buf')
        Just bid -> putStrLn ("Registered binding (id=" ++ show bid ++ ") for " ++ combo)
                      >> return (Map.insert bid action acc, buf')

-- ============================================================
-- Entry point
-- ============================================================

main :: IO ()
main = do
    let cfg = myConfig
    hSetBuffering stdout NoBuffering

    socketPath <- getEnv "WAYFIRE_SOCKET"
    sock <- socket AF_UNIX Stream defaultProtocol
    connect sock (SockAddrUnix socketPath)
    putStrLn "Connected to Wayfire IPC."

    SB.sendAll sock listOutputs
    (outputResp, buf0) <- readOneMsg sock B.empty
    outputs <- case parseOutputList outputResp of
      Right outs -> do
        let sorted = sortOn (\o -> o.geometry.x) outs
        putStrLn $ "Outputs: " ++ unwords (map (\o -> o.name ++ "(id=" ++ show o.id ++ " x=" ++ show o.geometry.x ++ ")") sorted)
        return sorted
      Left err -> do
        putStrLn $ "Warning: could not parse outputs: " ++ err
        putStrLn $ "Raw output response: " ++ BC.unpack outputResp
        return []

    let outputIds    = map (\o -> o.id) outputs
        outMap       = Map.fromList [(o.id, o.workarea) | o <- outputs]
        emptyState   = case outputIds of
                         [] -> SS.initialState cfg.workspaces cfg.layoutHook (1 :: Int)
                         _  -> SS.initialStateMulti cfg.workspaces cfg.layoutHook outputIds
        emptyWMState = WMState emptyState Map.empty outMap Set.empty

    SB.sendAll sock listViews
    (listResp, buf1) <- readOneMsg sock buf0
    startingState <- case parseIncomingMessage listResp of
      Right (Snapshot ws) -> do
        let live = filter (\w -> w.mapped && w.role /= "desktop-environment") ws
        putStrLn $ "Live windows: " ++ show (map (\w -> (w.id, w.outputId, w.title)) live)
        return $ synchronizeState ws emptyWMState
      _ -> return emptyWMState
    let ss0 = startingState.stackSet
    putStrLn $ "Initial current: ws=" ++ ss0.current.workspace.tag ++ " screen=" ++ show ss0.current.screenId
                ++ " wins=" ++ show (maybe [] SS.integrate ss0.current.workspace.stack)
    mapM_ (\sc -> putStrLn $ "  visible: ws=" ++ sc.workspace.tag ++ " screen=" ++ show sc.screenId
                              ++ " wins=" ++ show (maybe [] SS.integrate sc.workspace.stack))
          ss0.visible
    putStrLn "Window state initialized."

    SB.sendAll sock watchEvents
    (_, buf2) <- readOneMsg sock buf1
    putStrLn "Window event subscription active."

    (bm, buf3) <- registerBindings sock buf2 cfg.keys
    let env = WMEnv
          { wmSocket   = sock
          , bindingMap = bm
          , layoutHook = cfg.layoutHook
          , manageHook = cfg.manageHook
          }

    runSocketEngine env startingState buf3
