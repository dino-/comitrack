{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Model.Series
  ( PubStatus (..)
  , ReadingStatus (..)
  , formatModifiedDate
  , pubStatusOptionPairs
  , readingStatusOptionPairs
  )
  where

import ClassyPrelude.Yesod
import Control.Arrow ( (&&&) )
import Data.String.Conv ( toS )
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Data.Time.LocalTime ( TimeZone, utcToZonedTime )


data PubStatus = Ongoing | Ended
  deriving (Eq, Read, Show)
derivePersistField "PubStatus"


pubStatusOptionPairs :: [(Text, PubStatus)]
pubStatusOptionPairs = map (tshow &&& id) [Ongoing, Ended]


data ReadingStatus = Reading | NotReading
  deriving (Eq, Read, Show)
derivePersistField "ReadingStatus"


readingStatusOptionPairs :: [(Text, ReadingStatus)]
readingStatusOptionPairs = [("Reading", Reading), ("Not reading", NotReading)]


formatModifiedDate :: TimeZone -> UTCTime -> Text
formatModifiedDate z u = toS . formatTime defaultTimeLocale "%F %R"
  $ utcToZonedTime z u
