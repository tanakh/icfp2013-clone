module Handler.User where

import Import
import qualified Data.Text as T

getUserR :: Handler Html
getUserR = do
  Entity userId user' <- requireAuth

  user <- case user' of
    User { userIdent = uid, userAuthId = "" } -> do
      authId <- liftIO $ genAuthId userId uid
      runDB $ do
        update userId [ UserAuthId =. authId ]
        get404 userId
    _ -> do
      liftIO $ print user'
      return user'

  defaultLayout $ do
    setTitle "User settings"
    $(widgetFile "user")

postUserR :: Handler Html
postUserR = do
  Entity userId _ <- requireAuth
  mres <- lookupPostParam "input-name"
  liftIO $ print mres
  case mres of
    Just name | name /= "" && T.length name < 256 -> do
      runDB $ update userId [ UserName =. name ]
      setMessage [shamlet|<div.alert.alert-success>Update success|]
    _ ->
      setMessage [shamlet|<div.alert.alert-error>Please input valid name|]
  redirect UserR

getRankingR :: Handler Html
getRankingR = do
  ranking <- runDB $ zip [1 :: Int .. ] <$>
             selectList [ UserNumRequests >. 0 ] [ Desc UserScore ]
  defaultLayout $ do
    setTitle "Ranking"
    $(widgetFile "ranking")
