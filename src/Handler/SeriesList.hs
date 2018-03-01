{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.SeriesList where

import Yesod.Table ( Table )
import qualified Yesod.Table as Table

import Import


seriesTable :: Table site Series
seriesTable = mempty
  <> Table.text   "Title"               seriesTitle
  <> Table.text   "Source"              (maybe "" id . seriesSourceName)
  <> Table.text   "Publication status"  (tshow . seriesPubStatus)
  <> Table.text   "Reading status"      (tshow . seriesReadingStatus)
  <> Table.int    "Issues read"         seriesIssuesRead
  <> Table.text   "Last modified"       (tshow . seriesModified)


getSeriesListR :: Handler Html
getSeriesListR = do
  series <- runDB $ map entityVal <$> selectList [] [Asc SeriesFileAsTitle]
  defaultLayout $ Table.buildBootstrap seriesTable series
