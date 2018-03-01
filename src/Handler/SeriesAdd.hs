{-# LANGUAGE FlexibleContexts #-}
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
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import Import
import Model.Series ( pubStatusOptionPairs, readingStatusOptionPairs )


-- The GET handler displays the form
getSeriesAddR :: Handler Html
getSeriesAddR = do
  -- Generate the form to be displayed
  (widget, enctype) <- generateFormPost seriesAddForm
  defaultLayout
    [whamlet|
      <p>
      <form method=post action=@{SeriesAddR} enctype=#{enctype}>
        ^{widget}
        <button class="btn btn-primary active">Ok
        <a href="@{SeriesListR}" class="btn btn-primary">Cancel
    |]


seriesAddForm :: Form Series
seriesAddForm = do
  renderBootstrap3 BootstrapBasicForm $ Series
  --renderDivs $ Series
    <$> lift (liftIO getCurrentTime)
    <*> areq textField "title" Nothing
    -- This `pure ""` is a dummy value, should NEVER go into the db! Fixed in
    -- postSeriesAddR below.
    <*> pure ""
    <*> aopt textField "creators" Nothing
    <*> aopt textField "source name" (Just $ Just "")
    <*> aopt textField "source url" (Just $ Just "")
    <*> areq (selectFieldList pubStatusOptionPairs) "publication status" Nothing
    <*> areq (selectFieldList readingStatusOptionPairs) "reading status" Nothing
    <*> areq issuesReadField "issues read" ((Just 0) :: Maybe Int)

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
        <form method=post action=@{SeriesAddR} enctype=#{enctype}>
          ^{widget}
          <button class="btn btn-primary active">Ok
          <a href="@{SeriesListR}" class="btn btn-primary">Cancel
      |]


{- Generate the seriesFileAsTitle column value
   In practical terms, this means lower-case the entire title and remove "the "
   if present so that sorting works in a human-friendly way.
-}
genFileAs :: Text -> Text
genFileAs = rmThe . toLower where
  rmThe t = maybe t id $ stripPrefix "the " t
