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

import Data.Brooks.Vars
import qualified IO.Brooks.Database as DB

type DataFormat = [(String, DVar)]

data Store = Store DataFormat
    deriving (Typeable)

$(deriveSafeCopy 0 'base ''DVar)
$(deriveSafeCopy 0 'base ''Tuple)
$(deriveSafeCopy 0 'base ''Relation)
$(deriveSafeCopy 0 'base ''Heading)
-- $(deriveSafeCopy 0 'base 'DataFormat)
$(deriveSafeCopy 0 'base ''Store)

data AcidStateEngine st = AcidStateEngine (AcidState (DataFormat))

bindName :: String -> DVar -> Update Store ()
bindName name val = do Store pairs <- get
                       let m = fromList pairs
                       let newMap = insert name val m
                       let newPairs = toList newMap
                       put $ Store newPairs

value :: String -> Query Store (Maybe DVar)
value name = do Store pairs <- ask
                let res = lookup name pairs
                return res

$(makeAcidic ''Store ['bindName, 'value])

instance DB.Engine (AcidStateEngine a) where
    engineName _ = "Timothy - AcidState store for BrooksDB."

    bindName ase name value = do
        st <- onAcid ase
        update st (BindName name value)
        putStrLn $ "bound " ++ (show name) ++ " bound to " ++ (show value)
        return ()

    --close engine = closeAcidState store

--onAcid :: AcidStateEngine st -> IO (AcidState (DataFormat))
onAcid :: AcidStateEngine st -> IO (AcidState st)
onAcid (AcidStateEngine asdf) = return asdf

newDb :: FilePath -> IO (DB.Database (AcidStateEngine (AcidState DataFormat)))
newDb path = do
    store <- openLocalStateFrom path ([]::DataFormat)
    return ( DB.newDb ( AcidStateEngine store ) )

