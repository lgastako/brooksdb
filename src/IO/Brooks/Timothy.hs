{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}

module IO.Brooks.Timothy where

-- Timothy, an ACID store for Brooks.

import Data.Map             ( Map, insert, empty )
import Data.Acid            ( AcidState, Update, makeAcidic, update, openLocalStateFrom )
import Data.SafeCopy        ( base, deriveSafeCopy )
import Control.Monad.State  ( get, put )

import Data.Brooks.Vars     ( DVar )
import qualified IO.Brooks.Database as DB

instance SafeCopy DVar where
    --putCopy (Contacts contacts) = contain $ safePut contacts
    --getCopy = contain $ Contacts <$> safeGet

    getCopy = contain $ DVar <$> safeGet
    putCopy (RelVar x) = contain $ safePut x
    putCopy (TupVar x) = contain $ safePut x
    putCopy (BoolVar x) = contain $ safePut x
    putCopy (IntVar x) = contain $ safePut x
    putCopy (FloatVar x) = contain $ safePut x
    putCopy (DoubleVar x) = contain $ safePut x
    putCopy (StringVar x) = contain $ safePut x


data Store = Store (Map String DVar)

$(deriveSafeCopy 0 'base ''Store)

data AcidStateEngine st = AcidStateEngine (AcidState (Map String DVar))

bindName :: String -> DVar -> Update Store ()
bindName nam val = do Store map <- get
                      put $ Store (insert nam val map)

                      --putStrLn $ "BIND Name: " ++ (show nam) ++ " bound to " ++ (show val)

$(makeAcidic ''Store ['bindName])

instance DB.Engine (AcidStateEngine st) where
    engineName _ = "Timothy - AcidState store for BrooksDB."

    bindName engine name value = do
        update (onAcid engine) (BindName name value)

onAcid :: AcidStateEngine st -> IO (AcidState (Map String DVar))
onAcid (AcidStateEngine ast) = ast

newDb :: FilePath -> IO (DB.Database (AcidStateEngine st))
newDb path = do
    store <- openLocalStateFrom path (empty)
    DB.newDb AcidStateEngine

