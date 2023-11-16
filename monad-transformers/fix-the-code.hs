import Control.Monad.Trans.Maybe
import Control.Monad

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = MaybeT $ do -- add MaybeT data constructor to match the return type
  v <- getLine
  guard $ isValid v
  return (Just v) -- cast v to a Maybe before lifting it to IO as expected by MaybeT

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite -- add runMaybeT so '<-' unwraps the IO monad and not the MaybeT monad
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)
