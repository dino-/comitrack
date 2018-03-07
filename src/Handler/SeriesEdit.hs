{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.SeriesEdit
  ( getSeriesEditR
  , postSeriesEditR
  )
  where

import Import
import Model.Series ( genFileAs )
import SeriesHelper ( seriesForm, seriesFormEmpty )


-- The GET handler displays the form
getSeriesEditR :: SeriesId -> Handler Html
getSeriesEditR seriesId = do
  mSeries <- runDB $ get seriesId
  series <- maybe (redirect SeriesListR) return mSeries

  (widget, enctype) <- generateFormPost . seriesForm $ series
  defaultLayout
    [whamlet|
      <h3>Edit series
      <p>
      <form role=form method=post action=@{SeriesEditR seriesId} enctype=#{enctype}>
        ^{widget}
        <a href="@{SeriesListR}" .btn .btn-default>Cancel
        <button type="submit" .btn .btn-primary>Ok
    |]


-- The POST handler processes the form
postSeriesEditR :: SeriesId -> Handler Html
postSeriesEditR seriesId = do
  ((result, widget), enctype) <- runFormPost $ seriesFormEmpty
  case result of
    FormSuccess series -> do
      let fixedSeries = series { seriesFileAsTitle = genFileAs $ seriesTitle series }
      _ <- runDB $ replace seriesId fixedSeries
      redirect SeriesListR
    _ -> defaultLayout
      [whamlet|
        <p>Invalid input, let's try again.
        <form role=form method=post action=@{SeriesEditR seriesId} enctype=#{enctype}>
          ^{widget}
          <a href="@{SeriesListR}" .btn .btn-default>Cancel
          <button type="submit" .btn .btn-primary>Ok
      |]
