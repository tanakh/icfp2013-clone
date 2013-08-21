module Handler.API where

import Import
import Eval

import Data.Aeson.QQ
import qualified Data.Aeson.Generic
import qualified Data.Aeson.Types
import qualified Data.Text
import qualified Data.Attoparsec.Number
import Network.HTTP.Types.Status hiding (Status)
import qualified Database.Esqueleto as E

import Control.Lens
import Control.Lens.Aeson
import Control.Monad
import Data.Aeson
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lens (unpacked)
import System.Directory
import Data.List
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import Text.Printf
import Numeric.Lens
import Data.Vector.Lens
import System.FilePath
import System.Random
import Data.Maybe

problemsPerUser :: Int
problemsPerUser = 2000

authenticate :: Handler (Entity User)
authenticate = do
  mauth <- lookupGetParam "auth"
  case mauth of
    Nothing ->
      permissionDenied "authorization required"
    Just auth -> do
      muser <- runDB $ getBy (UniqueAuthId auth)
      case muser of
        Nothing ->
          permissionDenied "authorization required"
        Just euser -> do
          touchUser euser

getPostJson :: Handler (Maybe Value)
getPostJson = do
  t <- lookupPostParam "json"
  return (join $ fmap (decode' . encodeUtf8 . fromStrict) t :: Maybe Value)

window :: UTCTime -> Integer
window t = floor (utctDayTime t) `div` 20 * 20

touchUser :: Entity User -> Handler (Entity User)
touchUser (Entity userkey User {..}) = do
  when (userName == "") $ redirect UserR
  cur <- liftIO getCurrentTime
  case () of
    _ | window cur /= window userRequestWindow -> do
      Entity userkey <$>
        (runDB $ updateGet userkey [ UserRequestWindow =. cur
                                   , UserRequestAmount =. 1
                                   , UserNumRequests +=. 1
                                   ])
    _ | userRequestAmount >= 5 -> do
      runDB $ update userkey [ UserRequestWindow =. cur
                             , UserRequestAmount =. 1
                             , UserNumRequests +=. 1
                             ]
      sendResponseStatus (mkStatus 429 "too many requests") ("" :: Text)
    _ -> do
      Entity userkey <$> (runDB $ updateGet userkey [ UserRequestAmount +=. 1
                                                    , UserNumRequests +=. 1
                                                    ])

touchStatus :: Entity Status -> Handler ()
touchStatus (Entity skey stat) = do
  cur <- liftIO getCurrentTime
  when (isNothing $ statusFirstTry stat) $
    runDB $ update skey [ StatusFirstTry =. Just cur ]
  when (timeLeft cur stat <= 0) $
    sendResponseStatus (mkStatus 410 "Gone") ("time limit exceeded" :: Text)
  when (statusSolved stat) $
    sendResponseStatus (mkStatus 412 "Already Solved") ("" :: Text)

assignProblems :: Key User -> Handler ()
assignProblems userkey = do
  problems <- runDB $ selectList [] []
  selected <- liftIO $ randomSelect problemsPerUser (length problems) problems
  runDB $ forM_ selected $ \(Entity problemkey _) -> do
    insert_ $ Status { statusUser = userkey
                     , statusProblem = problemkey
                     , statusSolved = False
                     , statusFirstTry = Nothing
                     }
  where
    randomSelect _ _ [] = return []
    randomSelect rest total (x:xs) = do
      p <- randomRIO (0, 1)
      if p < (fromIntegral rest / fromIntegral total :: Double)
        then (x:) <$> randomSelect (rest - 1) total xs
        else randomSelect rest total xs

timeLeft :: UTCTime -> Status -> Double
timeLeft cur s =
  realToFrac (fromMaybe cur (statusFirstTry s) `diffUTCTime` cur) + 300

-- Equivalence Checker

-- Handlers

postMyProblemsR :: Handler Value
postMyProblemsR = do
  Entity userkey _ <- authenticate
  problems <- runDB $ E.select $ E.from $ \(s, p) -> do
    E.where_ (s E.^. StatusUser E.==. E.val userkey)
    E.where_ (s E.^. StatusProblem E.==. p E.^. ProblemId)
    return (s, p)

  if null problems
    then do
    assignProblems userkey
    postMyProblemsR
    else do
    cur <- liftIO getCurrentTime
    returnJson $ map (f cur) problems

  where
    f cur (Entity _ s@Status{..}, Entity _ Problem {..})
      | isNothing statusFirstTry = v
      | tl <= 0 || statusSolved =
        v & key "timeLeft"._Double.~ (max 0 tl)
          & key "solved"._Bool.~ statusSolved
      | otherwise =
        v & key "timeLeft"._Double.~ (max 0 tl)
     where
      tl = timeLeft cur s
      v = [aesonQQ|
           {
             "id": <|problemIdent|>,
             "size": <|problemSize|>,
             "operators": <|problemOperators|>
           } |]

postEvalR :: Handler Value
postEvalR = do
  Entity userkey User {..} <- authenticate
  mjs <- getPostJson

  (program, args, mstat) <-
    case ( mjs ^?  _Just.key "id"._String
         , mjs ^?  _Just.key "program"._String
         , mjs ^.. _Just.key "arguments"._Array.traversed._String.unpacked.to read) of
      (Just pid, Nothing, args) -> runDB $ do
          Entity pkey prob <- getBy404 $ UniqueProblem pid
          mstat            <- getBy    $ UniqueStatus userkey pkey
          return (problemChallenge prob, args, mstat)
      (Nothing, Just p, args) ->
        return (p, args, Nothing)
      _ ->
        invalidArgs []
  maybe (return ()) touchStatus mstat

  when (T.length program > 1024 || length args > 256) $
    sendResponseStatus (mkStatus 413 "request too big") ("" :: Text)

  results <- mapM (either (\e -> invalidArgs [T.pack e]) return) $
             map (evalString $ T.unpack program) args

  returnJson (map (printf "%016X") results :: [String])

postGuessR :: Handler Value
postGuessR = do
  Entity userkey User {..} <- authenticate
  mjs <- getPostJson

  (ans, guess, mstat) <-
    case ( mjs ^?  _Just.key "id"._String
         , mjs ^?  _Just.key "program"._String) of
      (Just pid, Just p) -> runDB $ do
        Entity pkey prob <- getBy404 $ UniqueProblem pid
        mstat            <- getBy    $ UniqueStatus userkey pkey
        return (problemChallenge prob, p, mstat)
      _ ->
        invalidArgs []
  maybe (return ()) touchStatus mstat

  when (T.length guess > 1024) $
    sendResponseStatus (mkStatus 413 "request too big") ("" :: Text)

  res <- liftIO $ equivString (T.unpack ans) (T.unpack guess)
  case res of
    Left e ->
      invalidArgs [T.pack e]

    Right Equivalent -> do
      runDB $ do
        case mstat of
          Nothing -> return ()
          Just (Entity skey _) -> do
            update userkey [ UserScore +=. 1 ]
            update skey    [ StatusSolved =. True ]
      returnJson [aesonQQ| { "status": "win" } |]

    Right (Mismatch x y z) -> do
      runDB $ update userkey [ UserMismatches +=. 1 ]
      returnJson [aesonQQ|
        {
          "status": "mismatch",
          "values": <|[x, y, z]|>
        }
      |]

    Right (Undecidable msg) -> returnJson [aesonQQ|
        {
          "status": "mismatch",
          "values": <|msg|>
        }
      |]

postTrainR :: Handler Value
postTrainR = do
  Entity userkey User {..} <- authenticate
  mjs <- getPostJson
  let spec = ( mjs ^?  _Just.key "size"._Integer.integral
             , mjs ^.. _Just.key "operators"._Array.traversed._String)

  let specOk = case spec of
        (Just 42,  []) -> True
        (Just 137, []) -> True
        (Nothing,  []) -> True
        (Just n,   []) -> n >= 3 && n <= 30
        (Just n, ["tfold"]) -> n >= 10
        (_,  ["tfold"]) -> True
        (Just n, ["fold" ]) -> n >= 11
        (_,  ["fold" ]) -> True
        _ -> False
  when (not specOk) $ invalidArgs ["bad request"]

  let satisfy Problem {..} = case spec of
        (Just 42,  _) ->
          problemSize <= 30 && "bonus" `elem` problemOperators
        (Just 137, _) ->
          problemSize >  30 && "bonus" `elem` problemOperators
        (size, oprs) ->
          satisfySize size &&
          satisfyOperator oprs &&
          "bonus" `notElem` problemOperators
        where
          satisfySize Nothing = True
          satisfySize (Just s) = problemSize == s
          satisfyOperator = all (`elem` problemOperators)

  mines <- runDB $ E.select $ E.from $ \(s, p) -> do
    E.where_ (s E.^. StatusUser E.==. E.val userkey)
    E.where_ (s E.^. StatusProblem E.==. p E.^. ProblemId)
    return p

  problems <- runDB $ selectList [] []
  let numProblems = length problems

  let go i = do
        when (i > 100) $ invalidArgs ["bad request"]
        p <- liftIO $ entityVal . (problems !!) <$> randomRIO (0, numProblems - 1)
        if satisfy p &&
           all (\q -> problemIdent p /= problemIdent (entityVal q)) mines
          then return p
          else go $ i + 1
  p <- go (0 :: Int)

  returnJson [aesonQQ|
    {
      "challenge": <|problemChallenge p|>,
      "id": <|problemIdent p|>,
      "size": <|problemSize p|>,
      "operators": <|problemOperators p|>
    }
  |]

postStatusR :: Handler Value
postStatusR = do
  Entity _ User {..} <- authenticate
  cur <- liftIO $ getCurrentTime
  let resetIn = max 0
                $ fromIntegral (window userRequestWindow + 20)
                - realToFrac (utctDayTime cur) :: Double
      score = 0 :: Int
      trainScore = 0 :: Int

  returnJson [aesonQQ|
  {
    "easyChairId": <|T.take 4 userAuthId|>,
    "auth": <|userAuthId|>,
    "contestScore": <|score|>,
    "trainingScore": <|trainScore|>,
    "mismatches": <|userMismatches|>,
    "numRequests": <|userNumRequests|>,
    "requestWindow": {
      "resetsIn": <|resetIn|>,
      "amount": <|userRequestAmount|>,
      "limit": 5
    },
    "cpuWindow": {
      "resetsIn": 0,
      "amount": 0,
      "limit": 0
    },
    "cpuTotalTime": 0
  }
  |]

-- for deploy

postRegisterR :: Handler Value
postRegisterR = do
  problemNum <- runDB $ count ([] :: [Filter Problem])
  when (problemNum > 0) $ invalidArgs ["already registered"]

  mjs <- getPostJson
  case mjs ^? _Just.key "dir"._String.unpacked of
    Nothing -> invalidArgs ["dir is not specified"]
    Just dir -> do
      registerProblems dir
      return [aesonQQ|{"result": "ok"}|]

registerProblems :: String -> Handler ()
registerProblems dir = do
  files <- liftIO $ getDirectoryContents dir
  forM_ (filter (".json" `isSuffixOf`) files) $ \file -> do
    mjs <- liftIO $ decode' <$> L.readFile (dir </> file)
    let mproblem = do
          js <- mjs :: Maybe Value
          Problem
            <$> (js ^? key "id"._String)
            <*> (js ^? key "size"._Integer.integral)
            <*> (do oprs <- js ^? key "operators"._Array.from vector
                    return $ oprs ^.. traverse._String)
            <*> (js ^? key "challenge"._String)
    case mproblem of
      Nothing -> liftIO $ printf "parse error: %s\n" file
      Just p -> runDB $ insert_ p

-- play ground

getPlayR :: Handler Html
getPlayR = do
  Entity _ user <- requireAuth
  defaultLayout $ do
    setTitle "Playground"
    $(widgetFile "play")

postPlayR :: Handler Value
postPlayR = do
  murl <- lookupPostParam "url"
  case murl of
    Just "myproblems" -> postMyProblemsR
    Just "eval"       -> postEvalR
    Just "guess"      -> postGuessR
    Just "train"      -> postTrainR
    Just "status"     -> postStatusR
    Just "register"   -> postRegisterR
    _ -> notFound