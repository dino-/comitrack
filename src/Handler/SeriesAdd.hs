{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.SeriesAdd
  ( getSeriesAddR
  , postSeriesAddR
  )
  where

import Data.Time.Clock ( getCurrentTime )
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), bfs, renderBootstrap3)

import Import
import Model.Series ( genFileAs, pubStatusOptionPairs, readingStatusOptionPairs )


-- The GET handler displays the form
getSeriesAddR :: Handler Html
getSeriesAddR = do
  -- Generate the form to be displayed
  (widget, enctype) <- generateFormPost seriesAddForm
  defaultLayout
    [whamlet|
      <h3>Add a new series
      <p>
      <form role=form method=post action=@{SeriesAddR} enctype=#{enctype}>
        ^{widget}
        <a href="@{SeriesListR}" .btn .btn-default>Cancel
        <button type="submit" .btn .btn-primary>Ok
    |]


seriesAddForm :: Form Series
seriesAddForm = do
  renderBootstrap3 BootstrapBasicForm $ Series
    <$> lift (liftIO getCurrentTime)
    <*> areq textField (bfs ("title" :: Text)) Nothing
    -- This `pure ""` is a dummy value, should NEVER go into the db! Fixed in
    -- postSeriesAddR below.
    <*> pure ""
    <*> aopt textField (bfs ("creators" :: Text)) Nothing
    <*> aopt textField (bfs ("source name" :: Text)) (Just $ Just "")
    <*> aopt textField (bfs ("source url" :: Text)) (Just $ Just "")
    <*> areq (selectFieldList pubStatusOptionPairs) (bfs ("publication status" :: Text)) Nothing
    <*> areq (selectFieldList readingStatusOptionPairs) (bfs ("reading status" :: Text)) Nothing
    <*> areq issuesReadField (bfs ("issues read" :: Text)) ((Just 0) :: Maybe Int)

  where
    issuesReadErrMsg :: Text
    issuesReadErrMsg = "Issues read must be >= 0"

    issuesReadField = checkBool (>= 0) issuesReadErrMsg intField


-- The POST handler processes the form
postSeriesAddR :: Handler Html
postSeriesAddR = do
  ((result, widget), enctype) <- runFormPost seriesAddForm
  case result of
    FormSuccess series -> do
      let fixedSeries = series { seriesFileAsTitle = genFileAs $ seriesTitle series }
      _ <- runDB $ insertEntity fixedSeries
      redirect SeriesListR
    _ -> defaultLayout
      [whamlet|
        <p>Invalid input, let's try again.
        <form role=form method=post action=@{SeriesAddR} enctype=#{enctype}>
          ^{widget}
          <a href="@{SeriesListR}" .btn .btn-default>Cancel
          <button type="submit" .btn .btn-primary>Ok
      |]
