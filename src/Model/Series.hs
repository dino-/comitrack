{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Model.Series
  ( PubStatus (..)
  , ReadingStatus (..)
  , pubStatusOptionPairs
  , readingStatusOptionPairs
  )
  where

import ClassyPrelude.Yesod


data PubStatus = InProduction | Ended
  deriving (Eq, Read, Show)
derivePersistField "PubStatus"


pubStatusOptionPairs :: [(Text, PubStatus)]
pubStatusOptionPairs = [("In production", InProduction) , ("Ended", Ended)]


data ReadingStatus = Reading | NotReading
  deriving (Eq, Read, Show)
derivePersistField "ReadingStatus"


readingStatusOptionPairs :: [(Text, ReadingStatus)]
readingStatusOptionPairs = [("Reading", Reading), ("Not reading", NotReading)]
