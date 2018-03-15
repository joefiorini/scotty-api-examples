{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


import GHC.Generics
import Control.Monad.IO.Class
import Web.Scotty
import Data.Text
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data Greeter = Greeter { lang :: String, greeting :: String } deriving (Show, Generic)

data Greeting = Greeting { userGreeting :: String } deriving (Show, Generic)

instance FromRow Greeter where
  fromRow = Greeter <$> field <*> field

instance ToJSON Greeting
instance FromJSON Greeting

greetUser :: Greeter -> String -> String
greetUser greeter name = greeting greeter ++ " " ++ name

defaultGreeter :: Greeter
defaultGreeter = Greeter {lang = "en", greeting = "Hello"}

getGreeter :: Connection -> Text -> IO (Maybe Greeter)
getGreeter connection language = do
  rows <- query connection "select lang, greeting from greetings where lang = ?"
    $ Only language
  return $ extractGreeting (rows :: [Greeter])
 where
  extractGreeting [greetingRow] = Just $ greetingRow
  extractGreeting _             = Nothing

main :: IO ()
main = scotty 4000 $ get "/greet/:lang/:name" $ do
  conn <- liftIO
    $ connectPostgreSQL "postgres://localhost/haskell-scotty-testing"
  lang     <- param "lang"
  name     <- param "name"
  greeterM <- liftIO $ getGreeter conn lang
  json Greeting {userGreeting = (greetUser (extractGreeter greeterM) name)}
 where
  extractGreeter (Just greeter) = greeter
  extractGreeter Nothing        = defaultGreeter




























