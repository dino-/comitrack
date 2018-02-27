{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.SeriesAdd where

import Data.Time.Clock
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import Import
--import Model


--seriesAddForm :: Form Series
seriesAddForm :: Html -> MForm Handler (FormResult Series, Widget)
--seriesAddForm = renderDivs $ Series
seriesAddForm = do
  renderBootstrap3 BootstrapBasicForm $ Series
    <$> lift (liftIO getCurrentTime)
    <*> areq textField "title" Nothing
    <*> aopt textField "creators" Nothing
    <*> aopt textField "source name" (Just $ Just "")
    <*> aopt textField "source url" (Just $ Just "")
    <*> areq intField "issues read" ((Just 0) :: Maybe Int)


-- The GET handler displays the form
getSeriesAddR :: Handler Html
getSeriesAddR = do
  -- Generate the form to be displayed
  (widget, enctype) <- generateFormPost seriesAddForm
  defaultLayout
    [whamlet|
      <p>
        The widget generated contains only the contents of the firm, not the form tag itself. So...
      <form method=post action=@{SeriesAddR} enctype=#{enctype}>
        ^{widget}
        <p>It also doesn't include the submit button.
        <button>Submit
    |]
--getSeriesAddR = error "Not yet implemented: getSeriesAddR"


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
--postSeriesAddR = error "Not yet implemented: postSeriesAddR"
