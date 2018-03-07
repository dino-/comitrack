{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SeriesHelper
  ( seriesForm
  , seriesFormEmpty
  )
  where

import Data.Time.Clock ( getCurrentTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), bfs, renderBootstrap3)

import Import
import Model.Series ( PubStatus (Ongoing), ReadingStatus (Reading),
  pubStatusOptionPairs, readingStatusOptionPairs )


emptySeries :: Series
emptySeries = Series
  { seriesModified = posixSecondsToUTCTime . realToFrac $ (0 :: Int)
  , seriesTitle = ""
  , seriesFileAsTitle = ""
  , seriesCreators = Nothing
  , seriesSourceName = Nothing
  , seriesSourceUrl = Nothing
  , seriesPubStatus = Ongoing
  , seriesReadingStatus = Reading
  , seriesIssuesRead = 0
  }


seriesForm :: Series -> Form Series
seriesForm series = do
  renderBootstrap3 BootstrapBasicForm $ Series
    <$> lift (liftIO getCurrentTime)
    <*> areq textField (bfs ("title" :: Text)) (Just . seriesTitle $ series)
    <*> pure ""  -- Dummy value, filled in by form post handlers
    <*> aopt textField (bfs ("creators" :: Text))
      (Just . seriesCreators $ series)
    <*> aopt textField (bfs ("source name" :: Text))
      (Just . seriesSourceName $ series)
    <*> aopt textField (bfs ("source url" :: Text))
      (Just . seriesSourceUrl $ series)
    <*> areq (selectFieldList pubStatusOptionPairs)
      (bfs ("publication status" :: Text)) (Just . seriesPubStatus $ series)
    <*> areq (selectFieldList readingStatusOptionPairs)
      (bfs ("reading status" :: Text)) (Just . seriesReadingStatus $ series)
    <*> areq issuesReadField (bfs ("issues read" :: Text))
      (Just . seriesIssuesRead $ series)

  where
    issuesReadErrMsg :: Text
    issuesReadErrMsg = "Issues read must be >= 0"

    issuesReadField = checkBool (>= 0) issuesReadErrMsg intField


seriesFormEmpty :: Form Series
seriesFormEmpty = seriesForm emptySeries
