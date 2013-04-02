module IO.Brooks.Timothy where

-- Timothy, an ACID store for Brooks.

import Data.Brooks.Vars ( DVar )

import Data.Map ( Map )

import qualified IO.Brooks.Database as DB

data AcidStateEngine

instance Engine AcidStateEngine where
    name = "Timothy - AcidState store for BrooksDB."

--data Store = Store Map


newDb :: DB.Database
newDb = DB.new AcidStateEngine

