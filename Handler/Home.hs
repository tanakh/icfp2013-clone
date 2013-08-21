module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  muser <- maybeAuth

  case muser of
    Just (Entity _ (User { userName = "" })) ->
      redirect UserR
    _ ->
      return ()

  defaultLayout $ do
    setTitle "ICFP Contest 2013 Unofficial Clone Server"
    $(widgetFile "homepage")
