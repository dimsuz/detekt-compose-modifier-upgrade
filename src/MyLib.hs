{-# LANGUAGE TemplateHaskell #-}

module MyLib (migrate) where

import qualified Control.Foldl as Fold
import Control.Lens (makeLenses)
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromJust, isJust)
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

-- IntMap.insert modPos optPos - 1 list; 135 -> 137
-- repeat (optPos - modPos - 1)
-- IntMap.insert (modPos + 1) modPos list; 136 -> 135
-- IntMap.insert (modPos + 2) (modPos + 1) list; 137 -> 136
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
            | isJust pos && hasOptionalParam = list <> reorderParams (fromJust pos) (lineNo + 1)
            | otherwise = list
       in ParseState modifierPosition swapPositions (lineNo + 1)
    done (ParseState pos list lineNo) = list

findModifier :: MonadIO io => Shell Line -> StateT ParseState io (IntMap Int)
findModifier inputLines = do
  s <- get
  liftIO $ putStrLn ("got state " <> show s)
  pure IntMap.empty

processFile :: MonadIO io => FilePath -> io ()
processFile path = do
  let inputLines = input path
  liftIO $ putStrLn ("processing file " <> path)
  swapPositions <- fold inputLines swapPositionsFold
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
