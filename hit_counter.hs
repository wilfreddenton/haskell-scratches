{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid
import           Data.Text.Lazy             (Text, pack)
import qualified Data.Text.Lazy             as TL
import           System.Environment         (getArgs)
import           Web.Scotty.Trans

data Config = Config {
  -- that's one, one click!
  -- two...two clicks!
  -- Three BEAUTIFUL clicks! ah ah ahhhh
  counts   :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

-- Stuff inside ScottyT is, except for things that escape
-- via IO, effectively read-only so we can't use StateT.
-- It would overcomplicate things to attempt to do so and
-- you should be using a proper database for production
-- applications.
type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = (M.insert k bump m, bump)
  where bump = fromMaybe 0 (m M.!? k) + 1

app :: Scotty ()
app = get "/:key" $ do
  (Config counts prefix) <- lift ask
  unprefixed <- param "key" :: Handler Text
  let key' = prefix <> unprefixed
  (newCounts, newInteger) <- liftIO $ bumpBoomp key' <$> readIORef counts
  liftIO $ writeIORef counts newCounts
  html $ mconcat [ "<h1>Success! Count was: "
                 , TL.pack $ show newInteger
                 , "</h1>"
                 ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter $ pack prefixArg
      runR r = runReaderT r config
  scottyT 3000 runR app
