{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.SeriesList where

import Data.Time.LocalTime ( TimeZone, getCurrentTimeZone )
import Colonnade
import qualified Text.Blaze.Html5.Attributes as HA
import Yesod.Colonnade

import Import
import Model.Series ( formatModifiedDate )


seriesTable :: TimeZone -> Colonnade Headed Series (Cell site)
seriesTable localTimeZone = mconcat
  [ headed "Title"               (textCell . seriesTitle)
  , headed "Source"              (textCell . maybe "" id . seriesSourceName)
  , headed "Publication status"  (textCell . tshow . seriesPubStatus)
  , headed "Reading status"      (textCell . tshow . seriesReadingStatus)
  , headed "Issues read"         (textCell . tshow . seriesIssuesRead)
  , headed "Last modified"       (textCell . formatModifiedDate localTimeZone . seriesModified)
  ]


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
