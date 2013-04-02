module IO.Brooks.Database ( Database
                          , Query
                          , Engine ( engineName
                                   , bindName
                                   , open
                                   )
                          , query
                          , newDb
                          ) where

import Data.Brooks.Vars ( DVar )


type Query = String -- For now


data (Engine a) => Database a = Database a
    deriving (Eq, Show)


class Engine a where
    engineName :: a -> String
    bindName :: a -> String -> DVar -> IO ()
    open :: a -> IO b



query :: Query -> DVar
query = undefined


newDb :: (Engine a) => a -> Database a
newDb = Database

