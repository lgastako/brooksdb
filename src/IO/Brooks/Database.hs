module IO.Brooks.Database ( Database ( Database )
                          , Query
                          , Engine ( engineName
                                   , bindName
                                   , value
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
    value :: a -> String -> IO (Maybe DVal)
    close :: a -> IO ()

query :: Query -> DVal
query = undefined

newDb :: (Engine a) => a -> Database a
newDb = Database
