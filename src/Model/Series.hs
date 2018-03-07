{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Model.Series
  ( PubStatus (..)
  , ReadingStatus (..)
  , formatModifiedDate
  , genFileAs
  , pubStatusOptionPairs
  , readingStatusHumanReadable
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


readingStatusHumanReadable :: ReadingStatus -> Text
readingStatusHumanReadable rs = maybe (tshow rs) id $ lookup rs rsMappedToText
  where
    rsMappedToText = [ (v, k) | (k, v) <- readingStatusOptionPairs ]


formatModifiedDate :: TimeZone -> UTCTime -> Text
formatModifiedDate z u = toS . formatTime defaultTimeLocale "%F %R"
  $ utcToZonedTime z u


{- Generate the seriesFileAsTitle column value
   In practical terms, this means lower-case the entire title and remove "the "
   if present so that sorting works in a human-friendly way.
-}
genFileAs :: Text -> Text
genFileAs = rmThe . toLower where
  rmThe t = maybe t id $ stripPrefix "the " t
