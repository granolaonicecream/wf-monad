{-# LANGUAGE OverloadedRecordDot #-}
module Layout
  ( SimpleFloat(..)
  , Tall(..)
  , TwoPane(..)
  , Resize(..)
  , IncMasterN(..)
  , NextLayout(..)
  , FirstLayout(..)
  , (|||)
  ) where

import Core
import qualified StackSet as SS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Wayfire (Geometry(..))

-- ---------------------------------------------------------------------------
-- SimpleFloat
-- ---------------------------------------------------------------------------

data SimpleFloat = SimpleFloat deriving (Show)

instance LayoutClass SimpleFloat where
    pureLayout _ _ stack gm =
        [ (wid, g) | wid <- SS.integrate stack, Just g <- [Map.lookup wid gm] ]

-- ---------------------------------------------------------------------------
-- Tall
-- ---------------------------------------------------------------------------

data Tall = Tall
  { tallNMaster :: Int
  , tallDelta   :: Rational
  , tallRatio   :: Rational
  } deriving (Show)

data Resize = Shrink | Expand deriving (Typeable, Show)
instance Message Resize

data IncMasterN = IncMasterN Int deriving (Typeable, Show)
instance Message IncMasterN

instance LayoutClass Tall where
    pureLayout (Tall nmaster _ ratio) screen stack _ =
        zip wins (tile ratio screen nmaster (length wins))
      where wins = SS.integrate stack

    handleMessage t m = return $
        case fromMessage m of
            Just Shrink          -> Just $ Layout t { tallRatio   = max 0 (tallRatio t - tallDelta t) }
            Just Expand          -> Just $ Layout t { tallRatio   = min 1 (tallRatio t + tallDelta t) }
            Nothing -> case fromMessage m of
                Just (IncMasterN d) -> Just $ Layout t { tallNMaster = max 0 (tallNMaster t + d) }
                Nothing             -> Nothing

-- ---------------------------------------------------------------------------
-- TwoPane
-- ---------------------------------------------------------------------------

data TwoPane = TwoPane
  { twoPaneDelta :: Rational
  , twoPaneRatio :: Rational
  } deriving (Show)

instance LayoutClass TwoPane where
    pureLayout (TwoPane _ ratio) screen stack _ =
        let (left, right) = splitH ratio screen
            wins          = SS.integrate stack
        in case wins of
             []            -> []
             [w]           -> [(w, screen)]
             (master:rest) -> (master, left) : map (\w -> (w, right)) rest

    handleMessage t m = return $
        case fromMessage m of
            Just Shrink -> Just $ Layout t { twoPaneRatio = twoPaneRatio t - twoPaneDelta t }
            Just Expand -> Just $ Layout t { twoPaneRatio = twoPaneRatio t + twoPaneDelta t }
            Nothing     -> Nothing

-- ---------------------------------------------------------------------------
-- Choose ((|||) combinator)
-- ---------------------------------------------------------------------------

data NextLayout  = NextLayout  deriving (Typeable, Show)
data FirstLayout = FirstLayout deriving (Typeable, Show)
instance Message NextLayout
instance Message FirstLayout

data Choose = Choose Bool Layout Layout deriving (Show)

instance LayoutClass Choose where
    runLayout (Choose False l _) = runLayout l
    runLayout (Choose True  _ r) = runLayout r

    handleMessage (Choose d l r) m
        | Just NextLayout <- fromMessage m =
            if not d
            then do
                ml <- handleMessage l (SomeMessage NextLayout)
                case ml of
                    Just l' -> return $ Just $ Layout $ Choose False l' r
                    Nothing -> do
                        mr <- handleMessage r (SomeMessage FirstLayout)
                        return $ Just $ Layout $ Choose True l (fromMaybe r mr)
            else do
                mr <- handleMessage r (SomeMessage NextLayout)
                case mr of
                    Just r' -> return $ Just $ Layout $ Choose True l r'
                    Nothing -> do
                        ml <- handleMessage l (SomeMessage FirstLayout)
                        return $ Just $ Layout $ Choose False (fromMaybe l ml) r
        | Just FirstLayout <- fromMessage m = do
            ml <- handleMessage l (SomeMessage FirstLayout)
            mr <- handleMessage r (SomeMessage FirstLayout)
            return $ Just $ Layout $ Choose False (fromMaybe l ml) (fromMaybe r mr)
        | otherwise = do
            let active = if d then r else l
            mNew <- handleMessage active m
            case mNew of
                Nothing   -> return Nothing
                Just newL -> return $ Just $ Layout $
                    if d then Choose True l newL else Choose False newL r

infixl 3 |||
(|||) :: Layout -> Layout -> Layout
l ||| r = Layout (Choose False l r)

-- ---------------------------------------------------------------------------
-- Geometry helpers
-- ---------------------------------------------------------------------------

tile :: Rational -> Geometry -> Int -> Int -> [Geometry]
tile ratio screen nmaster n
  | n <= nmaster || nmaster == 0 = splitV n screen
  | otherwise                    = splitV nmaster master ++ splitV (n - nmaster) slave
  where
    (master, slave) = splitH ratio screen

splitV :: Int -> Geometry -> [Geometry]
splitV n g
  | n <= 1    = [g]
  | otherwise = g { height = h } : splitV (n - 1) g { y = g.y + h, height = g.height - h }
  where h = g.height `div` n

splitH :: Rational -> Geometry -> (Geometry, Geometry)
splitH ratio g = (g { width = lw }, g { x = g.x + lw, width = g.width - lw })
  where lw = floor (fromIntegral g.width * ratio)
