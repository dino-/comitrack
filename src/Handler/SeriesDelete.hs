module Handler.SeriesDelete where

import Import


getSeriesDeleteR :: SeriesId -> Handler Html
getSeriesDeleteR seriesId = do
  runDB $ delete seriesId
  redirect SeriesListR
