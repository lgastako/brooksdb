{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}

module IO.Brooks.Timothy where

-- Timothy, an ACID store for Brooks.

import Data.Map             ( insert, fromList, toList )
import Data.Acid            ( AcidState
                            , Update
                            , Query
                            , makeAcidic
                            , update
                            , openLocalStateFrom
                            )
import Data.SafeCopy        ( base
                            , deriveSafeCopy
                            )
import Data.Typeable        ( Typeable )
import Control.Monad.State  ( get, put )
import Control.Monad.Reader ( ask )

import Data.Brooks.Vals
import qualified IO.Brooks.Database as DB

type DataFormat = [(String, DVal)]

data Store = Store DataFormat
    deriving (Typeable)

$(deriveSafeCopy 0 'base ''Store)

data AcidStateEngine st = AcidStateEngine (AcidState Store)

bindName :: String -> DVal -> Update Store ()
bindName name val = do Store pairs <- get
                       let m = fromList pairs
                       let newMap = insert name val m
                       let newPairs = toList newMap
                       put $ Store newPairs

value :: String -> Query Store (Maybe DVal)
value name = do Store pairs <- ask
                let res = lookup name pairs
                return res

$(makeAcidic ''Store ['bindName, 'value])

instance DB.Engine (AcidStateEngine a) where
    engineName _ = "Timothy - AcidState store for BrooksDB."

    close _ = return ()

    bindName ase name val = do
        acid <- onAcid ase
        _ <- update acid (BindName name val)
        putStrLn $ "bound " ++ (show name) ++ " bound to " ++ (show val)
        return ()
            where
                onAcid (AcidStateEngine acid) = return acid

newDb :: FilePath -> IO (DB.Database (AcidStateEngine (AcidState Store)))
newDb path = do
    acid <- openLocalStateFrom path (Store [])
    return ( DB.newDb ( AcidStateEngine acid ) )


withASE :: DB.Engine t1 =>
           DB.Database t1 -> (t1 -> t) -> t
withASE (DB.Database ase) f = f ase
