{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.SeriesList where

import Data.Time.LocalTime ( TimeZone, getCurrentTimeZone )
import Colonnade ( Colonnade, Headed, headed )
import qualified Text.Blaze.Html5.Attributes as HA
import Yesod.Colonnade ( Cell, cell, encodeCellTable, textCell )

import Import
import Model.Series ( formatModifiedDate )


seriesTable :: TimeZone -> Colonnade Headed Series (Cell site)
seriesTable localTimeZone = mconcat
  [ headed "Title"               (textCell . seriesTitle)
  , headed "Source"              mkSourceCell
  , headed "Publication status"  (textCell . tshow . seriesPubStatus)
  , headed "Reading status"      (textCell . tshow . seriesReadingStatus)
  , headed "Issues read"         (textCell . tshow . seriesIssuesRead)
  , headed "Last modified"       (textCell . formatModifiedDate localTimeZone . seriesModified)
  ]


mkSourceCell :: Series -> Cell site
mkSourceCell series = case (seriesSourceName series, seriesSourceUrl series) of
  (Just sourceName, Just sourceUrl) -> mkA sourceName sourceUrl
  (Just sourceName, Nothing       ) -> textCell sourceName
  (Nothing,         Just sourceUrl) -> mkA "source"   sourceUrl
  (Nothing,         Nothing       ) -> textCell ""

  where
    mkA :: Text -> Text -> Cell site
    mkA sourceName' sourceUrl' = cell [whamlet|<a href=#{sourceUrl'}>#{sourceName'}|]


getSeriesListR :: Handler Html
getSeriesListR = do
  localTimeZone <- liftIO getCurrentTimeZone
  series <- runDB $ map entityVal <$> selectList [] [Asc SeriesFileAsTitle]
  defaultLayout
    [whamlet|
      <p>
      <a href="@{SeriesAddR}" class="btn btn-primary active">Add a new series
      ^{encodeCellTable (HA.class_ "table table-striped") (seriesTable localTimeZone) series}
    |]
