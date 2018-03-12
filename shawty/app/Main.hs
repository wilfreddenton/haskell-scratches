{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad          (replicateM)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BC
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy         as TL
import qualified Database.Redis         as R
import           Network.URI            (URI, parseURI)
import qualified System.Random          as SR
import           Web.Scotty

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  randI <- SR.randomRIO (0, length xs - 1)
  return $ xs !! randI

shortyGen :: IO String
shortyGen = replicateM 7 $ randomElement alphaNum

saveURI :: R.Connection
        -> BC.ByteString
        -> BC.ByteString
        -> IO (Either R.Reply R.Status)
saveURI conn shortUri uri = R.runRedis conn $ R.set shortUri uri

getURI :: R.Connection
       -> BC.ByteString
       -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortUri = R.runRedis conn $ R.get shortUri

linkShorty :: String -> String
linkShorty shorty =
  concat [ "<a href=\""
         , shorty
         , "\">Copy and paste your short URL</a>"
         ]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat [ " shorty is: ", TL.pack (linkShorty shawty) ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat [ uri
            , " wasn't a url, did you forget http://?" ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

app :: R.Connection
    -> ScottyM ()
app rConn = do
  get "/" $ do
    uri <- param "uri"
    let parseUri = parseURI $ TL.unpack uri
    case parseUri of
      Nothing -> text $ shortyAintUri uri
      Just _ -> do
        shawty <- liftIO shortyGen
        let shorty = BC.pack shawty
        clashUri <- liftIO $ getURI rConn shorty
        case clashUri of
          Left reply -> text $ (TL.pack . show) reply
          Right Nothing -> do
            resp <- liftIO $ saveURI rConn shorty (encodeUtf8 $ TL.toStrict uri)
            html $ shortyCreated resp shawty
          Right (Just bs) -> text "the generated shorty clashes with an existing one please try again"

  get "/:shorty" $ do
    shorty <- param "shorty"
    uri <- liftIO $ getURI rConn shorty
    case uri of
      Left reply      -> text $ (TL.pack . show) reply
      Right Nothing   -> text "uri not found"
      Right (Just bs) -> html $ (shortyFound . TL.fromStrict . decodeUtf8) bs

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 $ app rConn

