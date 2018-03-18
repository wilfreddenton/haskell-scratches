{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Monoid

rDec :: Num a => Reader a a
rDec = ReaderT $ Identity . ((-) 1)


rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show


rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> do
  putStrLn $ "Hi: " <> show a
  return (a + 1)


sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  let sStr = show s
  putStrLn $ "Hi: " <> sStr
  return (sStr, s + 1)


isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e  -> putStrLn ("Good, was very excite: " <> e)
