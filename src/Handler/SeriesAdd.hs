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

import Import
import Model.Series ( genFileAs )
import SeriesHelper ( seriesFormEmpty )


-- The GET handler displays the form
getSeriesAddR :: Handler Html
getSeriesAddR = do
  (widget, enctype) <- generateFormPost seriesFormEmpty
  defaultLayout
    [whamlet|
      <h3>Add a new series
      <p>
      <form role=form method=post action=@{SeriesAddR} enctype=#{enctype}>
        ^{widget}
        <a href="@{SeriesListR}" .btn .btn-default>Cancel
        <button type="submit" .btn .btn-primary>Ok
    |]


-- The POST handler processes the form
postSeriesAddR :: Handler Html
postSeriesAddR = do
  ((result, widget), enctype) <- runFormPost seriesFormEmpty
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
