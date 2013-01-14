{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
import Web.Scotty

import Data.Default (def)
import Data.Typeable
import qualified Data.Map as Map

-- Network
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Handler.Warp (settingsPort)
import Network.HTTP.Types  (status404, status200, status201)

-- Database
import Data.Acid
import Data.SafeCopy
import qualified Data.Map as M
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import qualified Control.Monad.State as S
import Control.Lens (makeLenses, view, over)

------------------------------------------------------------------------------

type Key = String
type Value = String

data Database = Database !(Map.Map Key Value)
    deriving (Show, Ord, Eq, Typeable)

$(deriveSafeCopy 0 'base ''Database)
makeLenses ''Database

------------------------------------------------------

insertKey :: Key -> Value -> Update Database ()
insertKey key value
    = do Database m <- S.get
         S.put (Database (Map.insert key value m))

lookupKey :: Key -> Query Database (Maybe Value)
lookupKey key
    = do Database m <- ask
         return (Map.lookup key m)

deleteKey :: Key -> Update Database ()
deleteKey key
    = do Database m <- S.get
         S.put (Database (Map.delete key m))

allKeys :: Int -> Query Database [(Key, Value)]
allKeys limit
    = do Database m <- ask
         return $ take limit (M.toList m)

$(makeAcidic ''Database ['insertKey, 'lookupKey, 'allKeys, 'deleteKey])

------------------------------------------------------------------------------

config :: Options
config = def { verbose = 0
           , settings = (settings def) { settingsPort = 4000 }
           }

fixtures :: M.Map String String
fixtures = Map.empty

------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Loading Database."
    database <- openLocalStateFrom "db/" (Database fixtures)

    putStrLn "Starting HTTP Server."
    scottyOpts config $ do
        --middleware logStdoutDev
        middleware $ staticPolicy (noDots >-> addBase "static")

        get "/" $ file "index.html"

        get "/read/" $ do
            result <- liftIO $ query database (AllKeys 10)
            json result

        get "/read/:key" $ do
            key <- param "key"
            result <- liftIO $ query database (LookupKey key)
            case result of
               Nothing    -> status status404
               Just value -> json value

        get "/delete/:key" $ do
            key <- param "key"
            liftIO $ update database (DeleteKey key)
            status status200

        post "/update/" $ do
            (key, val) <- jsonData
            liftIO $ update database (InsertKey key val)
            status status200

------------------------------------------------------------------------------
