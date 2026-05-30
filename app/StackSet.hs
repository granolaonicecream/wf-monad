module StackSet where

import qualified Data.Set as Set
import Data.Set (Set)

data Stack a = Stack
  { focus :: a
  , up    :: [a]
  , down  :: [a]
  } deriving (Show, Eq)

integrate :: Stack a -> [a]
integrate (Stack f u d) = reverse u ++ [f] ++ d

differentiate :: [a] -> Maybe (Stack a)
differentiate []     = Nothing
differentiate (x:xs) = Just $ Stack x [] xs

filterStack :: (a -> Bool) -> Maybe (Stack a) -> Maybe (Stack a)
filterStack p = (>>= differentiate . filter p . integrate)

data Workspace i l a = Workspace
  { tag    :: i
  , layout :: l
  , stack  :: Maybe (Stack a)
  } deriving (Show)

data Screen i l a sid = Screen
  { workspace :: Workspace i l a
  , screenId  :: sid
  } deriving (Show)

data StackSet i l a sid = StackSet
  { current  :: Screen i l a sid
  , visible  :: [Screen i l a sid]
  , hidden   :: [Workspace i l a]
  , floating :: Set a
  } deriving (Show)

-- | Remove a window from all workspaces and the floating set.
delete :: (Eq a, Ord a) => a -> StackSet i l a sid -> StackSet i l a sid
delete wid ss = ss
  { current  = delFromScreen (current ss)
  , visible  = map delFromScreen (visible ss)
  , hidden   = map delFromWs (hidden ss)
  , floating = Set.delete wid (floating ss)
  }
  where
    go (Stack f u d)
      | f == wid  = case filter (/= wid) d of
                      (h:t) -> Just $ Stack h (filter (/= wid) u) t
                      []    -> case reverse (filter (/= wid) u) of
                                 (h:t) -> Just $ Stack h [] t
                                 []    -> Nothing
      | otherwise = Just $ Stack f (filter (/= wid) u) (filter (/= wid) d)
    delFromWs ws     = ws { stack = stack ws >>= go }
    delFromScreen sc = sc { workspace = delFromWs (workspace sc) }

-- | Generate a clean initial state with a single screen.
initialState :: Ord a => [i] -> l -> sid -> StackSet i l a sid
initialState [] _ _ = error "Must provide at least one workspace tag"
initialState (firstTag:tags) defLayout initialSid = StackSet
  { current  = Screen (Workspace firstTag defLayout Nothing) initialSid
  , visible  = []
  , hidden   = map (\t -> Workspace t defLayout Nothing) tags
  , floating = Set.empty
  }

-- | Generate a clean initial state for multiple screens.
initialStateMulti :: Ord a => [i] -> l -> [sid] -> StackSet i l a sid
initialStateMulti []     _         _      = error "Must provide at least one workspace tag"
initialStateMulti _      _         []     = error "Must provide at least one screen"
initialStateMulti (t:ts) defLayout (s:ss) = StackSet
  { current  = Screen (Workspace t defLayout Nothing) s
  , visible  = zipWith (\t' s' -> Screen (Workspace t' defLayout Nothing) s') visTags ss
  , hidden   = map (\t' -> Workspace t' defLayout Nothing) hidTags
  , floating = Set.empty
  }
  where
    (visTags, hidTags) = splitAt (length ss) ts

-- | Insert a window into the workspace with the given tag without switching to it.
insertInto :: Eq i => i -> a -> StackSet i l a sid -> StackSet i l a sid
insertInto t win ss
  | tag (workspace (current ss)) == t =
      ss { current = pushOnScreen (current ss) }
  | otherwise =
      case break (\sc -> tag (workspace sc) == t) (visible ss) of
        (pre, sc:post) -> ss { visible = pre ++ pushOnScreen sc : post }
        _              -> ss { hidden = map (\ws -> if tag ws == t then pushOnWs ws else ws) (hidden ss) }
  where
    pushOnScreen sc = sc { workspace = pushOnWs (workspace sc) }
    pushOnWs ws = ws { stack = case stack ws of
      Nothing            -> Just $ Stack win [] []
      Just (Stack f u d) -> Just $ Stack f u (d ++ [win]) }

-- | Make the screen with the given ID the current (focused) screen.
focusScreen :: Eq sid => sid -> StackSet i l a sid -> StackSet i l a sid
focusScreen sid ss
  | screenId (current ss) == sid = ss
  | otherwise =
      case break (\sc -> screenId sc == sid) (visible ss) of
        (pre, target:post) ->
          ss { current = target
             , visible = pre ++ current ss : post
             }
        _ -> ss

-- | Switch the current screen to the workspace with the given tag.
view :: Eq i => i -> StackSet i l a sid -> StackSet i l a sid
view t ss
  | tag (workspace (current ss)) == t = ss
  | otherwise =
      case break (\w -> tag w == t) (hidden ss) of
        (pre, target:post) ->
          ss { current = (current ss) { workspace = target }
             , hidden  = pre ++ workspace (current ss) : post
             }
        _ -> ss

-- | Swap the focused window with the one above it, wrapping at the top.
swapUp :: Stack a -> Stack a
swapUp (Stack f []     d) = Stack f (reverse d) []
swapUp (Stack f (u:us) d) = Stack f us (u:d)

-- | Swap the focused window with the one below it, wrapping at the bottom.
swapDown :: Stack a -> Stack a
swapDown (Stack f u     []) = Stack f [] (reverse u)
swapDown (Stack f u (d:ds)) = Stack f (d:u) ds

-- | Rotate the stack on whichever screen contains the window so that it becomes the focus.
focusWindow :: Eq a => a -> StackSet i l a sid -> StackSet i l a sid
focusWindow wid ss = ss
  { current = applyToScreen (current ss)
  , visible = map applyToScreen (visible ss)
  }
  where
    applyToScreen sc = sc { workspace = applyToWs (workspace sc) }
    applyToWs ws = ws { stack = fmap rotateToFocus (stack ws) }
    rotateToFocus stk
      | focus stk == wid = stk
      | otherwise = case break (== wid) (integrate stk) of
          (before, _:after) -> Stack wid (reverse before) after
          _                 -> stk

-- | Like 'view', but if the tag is currently visible on another screen,
-- steals it: that screen gets the current workspace in exchange.
greedyView :: Eq i => i -> StackSet i l a sid -> StackSet i l a sid
greedyView t ss
  | tag (workspace (current ss)) == t = ss
  | otherwise =
      case break (\w -> tag w == t) (hidden ss) of
        (pre, target:post) ->
          ss { current = (current ss) { workspace = target }
             , hidden  = pre ++ workspace (current ss) : post
             }
        _ ->
          case break (\sc -> tag (workspace sc) == t) (visible ss) of
            (pre, targetSc:post) ->
              ss { current = (current ss) { workspace = workspace targetSc }
                 , visible = pre ++ targetSc { workspace = workspace (current ss) } : post
                 }
            _ -> ss
