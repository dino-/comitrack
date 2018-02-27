{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.SeriesAdd where

import Data.Time.Clock ( getCurrentTime )
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import Import
import Model.Series ( pubStatusOptionPairs, readingStatusOptionPairs )


seriesAddForm :: Form Series
seriesAddForm = do
  renderBootstrap3 BootstrapBasicForm $ Series
  --renderDivs $ Series
    <$> lift (liftIO getCurrentTime)
    <*> areq textField "title" Nothing
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
        <button>Submit
    |]


-- The POST handler processes the form
postSeriesAddR :: Handler Html
postSeriesAddR = do
  ((result, widget), enctype) <- runFormPost seriesAddForm
  case result of
    FormSuccess series -> defaultLayout [whamlet|<p>#{show series}|]
    _ -> defaultLayout
      [whamlet|
        <p>Invalid input, let's try again.
        <form method=post action=@{SeriesAddR} enctype=#{enctype}>
          ^{widget}
          <button>Submit
      |]
