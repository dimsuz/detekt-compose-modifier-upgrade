module MyLib (migrate) where

import qualified Control.Foldl as Fold
import Turtle

processFile :: MonadIO io => FilePath -> io ()
processFile path = liftIO $ putStrLn ("processing file " <> path)

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
