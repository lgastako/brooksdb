{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}

module IO.Brooks.Timothy where

-- Timothy, an ACID store for Brooks.

import Data.Map             ( Map, insert, fromList, toList )
import Data.Acid            ( AcidState
                            , Update
                            , Query
                            , makeAcidic
                            , update
                            , openLocalStateFrom
                            )
import Data.SafeCopy        ( SafeCopy
                            , base
                            , contain
                            , safePut
                            , safeGet
                            , deriveSafeCopy )
import Data.Typeable        ( Typeable )
import Control.Monad        ( liftM )
import Control.Monad.State  ( get, put )
import Control.Monad.Reader ( ask )
import Control.Applicative  ( (<$>) )
import Data.Relation.Types  ( Relation
                            , Tuple
                            , Heading
                            )

import Data.Brooks.Vals
import qualified IO.Brooks.Database as DB

type DataFormat = [(String, DVal)]

data Store = Store DataFormat
    deriving (Typeable)

$(deriveSafeCopy 0 'base ''DVal)
$(deriveSafeCopy 0 'base ''Tuple)
$(deriveSafeCopy 0 'base ''Relation)
$(deriveSafeCopy 0 'base ''Heading)
-- $(deriveSafeCopy 0 'base 'DataFormat)
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

    bindName ase name value = do
        acid <- onAcid ase
        update acid (BindName name value)
        putStrLn $ "bound " ++ (show name) ++ " bound to " ++ (show value)
        return ()
            where
                onAcid (AcidStateEngine acid) = return acid

    --close engine = closeAcidState store


newDb :: FilePath -> IO (DB.Database (AcidStateEngine (AcidState Store)))
newDb path = do
    acid <- openLocalStateFrom path (Store [])
    return ( DB.newDb ( AcidStateEngine acid ) )

