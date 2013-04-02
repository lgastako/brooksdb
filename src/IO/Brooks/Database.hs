module IO.Brooks.Database ( Database
                          , Query
                          , Engine ( engineName
                                   , bindName
                                   , close
                                   )
                          , query
                          , newDb
                          ) where

import Data.Brooks.Vals ( DVal )


type Query = String -- For now


data (Engine a) => Database a = Database a
    deriving (Eq, Show)


class Engine a where
    engineName :: a -> String
    bindName :: a -> String -> DVal -> IO ()
    close :: a -> IO ()



query :: Query -> DVal
query = undefined


newDb :: (Engine a) => a -> Database a
newDb = Database

