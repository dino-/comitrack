{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Model.Series
  ( PubStatus (..)
  , pubStatusOptionPairs
  )
  where

import ClassyPrelude.Yesod


data PubStatus = InProduction | Ended
  deriving (Eq, Read, Show)
derivePersistField "PubStatus"


pubStatusOptionPairs :: [(Text, PubStatus)]
pubStatusOptionPairs = [("In production", InProduction) , ("Ended", Ended)]
