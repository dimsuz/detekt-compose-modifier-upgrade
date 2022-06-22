{-# LANGUAGE TemplateHaskell #-}

module MyLib (migrate) where

import qualified Control.Foldl as Fold
import Control.Lens (makeLenses, snoc)
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.Maybe (fromJust, isJust)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Turtle

findSwappableModifier :: Shell Line -> Maybe (Int, Int)
findSwappableModifier lines = Nothing

data ParseState = ParseState
  { _modifierPosition :: Maybe Int,
    _swapPositions :: IntMap Int,
    _currentPosition :: Int
  }
  deriving (Show)

makeLenses ''ParseState

-- First move the modifier line to be above the first optional parameter,
-- and then move each line below the modifier up to one line.
-- If modifier is already above the first optional parameter then do nothing
reorderParams :: Int -> Int -> IntMap Int
reorderParams modPos optPos
  | modPos == optPos - 1 = IntMap.empty
  | otherwise =
    let intermediateReorders = map (\i -> (modPos + i, modPos + i - 1)) [1 .. (optPos - modPos - 1)]
     in IntMap.singleton modPos (optPos - 1) <> IntMap.fromList intermediateReorders

swapPositionsFold :: Fold Line (IntMap Int)
swapPositionsFold = Fold step begin done
  where
    begin = ParseState Nothing IntMap.empty 0
    step (ParseState pos list lineNo) line =
      let hasModifier = not . null $ match (has "modifier: Modifier") (lineToText line)
          isDeclarationEnd = not . null $ match (suffix ") {") (lineToText line)
          hasOptionalParam
            | hasModifier && isDeclarationEnd = False -- happens when on the same line!
            | isJust pos && not isDeclarationEnd && (not . null $ match (has "=") (lineToText line)) = True
            | otherwise = False
          modifierPosition
            | hasModifier && isDeclarationEnd = Nothing -- happens when on the same line!
            | hasModifier = Just (lineNo + 1)
            | isDeclarationEnd = Nothing -- reset when at {
            | hasOptionalParam = Nothing -- reset when found optional
            | otherwise = pos
          swapPositions
            -- modifier + optional params
            | isJust pos && hasOptionalParam = list <> reorderParams (fromJust pos) (lineNo + 1)
            -- modifier + only required params
            | isJust pos && isDeclarationEnd = list <> reorderParams (fromJust pos) (lineNo + 1)
            | otherwise = list
       in ParseState modifierPosition swapPositions (lineNo + 1)
    done (ParseState pos list lineNo) = list

swappedOrder :: IntMap Int -> Int -> Int -> Ordering
swappedOrder ps line1 line2 = compare (IntMap.findWithDefault line1 line1 ps) (IntMap.findWithDefault line2 line2 ps)

processFile :: MonadIO io => FilePath -> io ()
processFile path = do
  let inputLines = input path
  liftIO $ putStrLn ("processing file " <> path)
  swapPositions <- fold inputLines swapPositionsFold
  unless (null swapPositions) $ do
    list <- zip [1 ..] <$> fold (lineToText <$> inputLines) Fold.list
    let sorted = map (unsafeTextToLine . snd) $ List.sortBy (\l1 l2 -> swappedOrder swapPositions (fst l1) (fst l2)) list
    update (\_ -> select sorted) path
  -- liftIO $ unless (null $ match (ends "TopAppBar.kt") (format fp path)) (T.putStrLn (mconcat (map snd sorted)))
  --sortBy (swappedOrder swapPositions) inputLines
  -- swapPositions <- evalStateT (findModifier inputLines) (ParseState Nothing IntMap.empty 0)
  liftIO $ putStrLn ("  swap positions " <> show swapPositions)

hasComposablesWithModifier :: FilePath -> IO Bool
hasComposablesWithModifier path = do
  case extension path of
    Just ext -> not <$> fold (grep (has "modifier: Modifier") (input path)) Fold.null
    Nothing -> pure False

migrate :: IO ()
migrate = do
  dir <- pwd
  sh $ do
    kotlinFile <- find (ends ".kt") dir
    useable <- liftIO $ hasComposablesWithModifier kotlinFile
    when useable $ processFile kotlinFile
