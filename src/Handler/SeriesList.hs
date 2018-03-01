{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.SeriesList where

import Data.Time.LocalTime ( TimeZone, getCurrentTimeZone )
import Yesod.Table ( Table )
import qualified Yesod.Table as Table

import Import
import Model.Series ( formatModifiedDate )


seriesTable :: TimeZone -> Table site Series
seriesTable localTimeZone = mempty
  <> Table.text   "Title"               seriesTitle
  <> Table.text   "Source"              (maybe "" id . seriesSourceName)
  <> Table.text   "Publication status"  (tshow . seriesPubStatus)
  <> Table.text   "Reading status"      (tshow . seriesReadingStatus)
  <> Table.int    "Issues read"         seriesIssuesRead
  <> Table.text   "Last modified"       (formatModifiedDate localTimeZone . seriesModified)


getSeriesListR :: Handler Html
getSeriesListR = do
  localTimeZone <- liftIO getCurrentTimeZone
  series <- runDB $ map entityVal <$> selectList [] [Asc SeriesFileAsTitle]
  defaultLayout
    [whamlet|
      <p>
      <a href="@{SeriesAddR}" class="btn btn-primary active">Add a new series
      ^{Table.buildBootstrap (seriesTable localTimeZone) series}
    |]
