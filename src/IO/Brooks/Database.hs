module IO.Brooks.Database where

import Data.Brooks.
import Data.Brooks.Vars ( DVar )

type Query = String -- For now

data Database = Database Engine

class Engine a where
    name :: String


query :: Query -> DVar
query = undefined


newDb :: (Engine a) => a -> Database
newDb = Database

