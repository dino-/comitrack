{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.SeriesList where

import Data.Time.LocalTime ( TimeZone, getCurrentTimeZone )
import Colonnade ( Colonnade, Headed, headed )
import qualified Text.Blaze.Html5.Attributes as HA
import Yesod.Colonnade ( Cell, cell, encodeCellTable, textCell )

import Import
import Model.Series ( formatModifiedDate, readingStatusHumanReadable )


seriesTable :: TimeZone -> Colonnade Headed (Entity Series) (Cell App)
seriesTable localTimeZone = mconcat
  [ headed "Title"               (textCell . seriesTitle . entityVal)
  , headed ""                    mkMenuCell
  , headed "Source"              (mkSourceCell . entityVal)
  , headed "Publication status"  (textCell . tshow . seriesPubStatus . entityVal)
  , headed "Reading status"      (textCell . readingStatusHumanReadable . seriesReadingStatus . entityVal)
  , headed "Issues read"         (textCell . tshow . seriesIssuesRead . entityVal)
  , headed "Last modified"       (textCell . formatModifiedDate localTimeZone . seriesModified . entityVal)
  ]


mkMenuCell :: Entity Series -> Cell App
mkMenuCell series = cell [whamlet|
    <div class="dropdown">
      <span class="glyphicon glyphicon-option-vertical dropdown-toggle" type="button" id="dropdownMenu1" data-toggle="dropdown" aria-haspopup="true" aria-expanded="true" aria-hidden="true">
      <ul class="dropdown-menu" aria-labelledby="dropdownMenu1">
        <li><a href="#">Edit</a>
        <li><a href="#" data-toggle="modal" data-target="#delModal" data-seriestitle="#{seriesTitle (entityVal series)}" data-delurl="@{SeriesDeleteR (entityKey series)}">Delete</a>
  |]


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
  eseries <- runDB $ selectList [] [Asc SeriesFileAsTitle]

  defaultLayout $ do
    toWidget [hamlet|
      <div class="modal fade" id="delModal" tabindex="-1" role="dialog" aria-laballedby="delModalLabel">
        <div class="modal-dialog" role="document">
          <div class="modal-content">
            <div class="modal-header">
              <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;
              <h4 class="modal-title">Confirm series delete
            <div class="modal-body">
              <p id="delbody">Dummy dialog text
            <div class="modal-footer">
              <button type="button" class="btn btn-default" data-dismiss="modal">Cancel
              <a id="delbutton" href="#" type="button" class="btn btn-primary">Delete
    |]

    toWidget [julius|
      $('#delModal').on('show.bs.modal', function (event) {
        var modal = $(this)

        // Link that triggered the modal
        var link = $(event.relatedTarget)

        // Set the body text
        var seriesTitle = link.data('seriestitle')
        modal.find('#delbody').text(
          "".concat("Delete '", seriesTitle, "', are you sure?"))

        // Set the Delete button's url
        var delUrl = link.data('delurl')
        modal.find('#delbutton').attr('href', delUrl)
      })
    |]

    [whamlet|
      <p>
      <a href="@{SeriesAddR}" .btn .btn-default>Add a new series
      ^{encodeCellTable (HA.class_ "table table-striped") (seriesTable localTimeZone) eseries}
    |]
