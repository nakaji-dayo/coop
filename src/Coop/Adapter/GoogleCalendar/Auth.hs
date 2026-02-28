module Coop.Adapter.GoogleCalendar.Auth
  ( runGoogleAuth
  , loadAndRefreshToken
  ) where

import Coop.Adapter.GoogleCalendar.Types (GoogleTokenResponse (..), GoogleToken (..))
import Coop.Config (GoogleCalendarConfig (..))
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, forkIO)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime, addUTCTime)
import Network.HTTP.Client (Manager, parseRequest, httpLbs, method, requestHeaders,
                            requestBody, RequestBody (RequestBodyLBS), responseBody, responseStatus)
import Network.HTTP.Types.Status (statusCode)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath (takeDirectory, (</>))
import System.Process (callCommand)

-- | Scopes needed for Google Calendar read access
calendarScope :: Text
calendarScope = "https://www.googleapis.com/auth/calendar.readonly"

-- | Run interactive OAuth flow: open browser, receive callback, save token
runGoogleAuth :: GoogleCalendarConfig -> Manager -> IO ()
runGoogleAuth cfg manager = do
  let clientId = googleClientId cfg
      clientSecret = googleClientSecret cfg
      redirectUri = "http://localhost:8085/callback" :: Text
      authUrl = T.concat
        [ "https://accounts.google.com/o/oauth2/v2/auth"
        , "?client_id=", clientId
        , "&redirect_uri=", redirectUri
        , "&response_type=code"
        , "&scope=", calendarScope
        , "&access_type=offline"
        , "&prompt=consent"
        ]

  TIO.putStrLn $ "Opening browser for Google authorization..."
  TIO.putStrLn $ "If the browser doesn't open, visit: " <> authUrl
  callCommand $ "open " <> show (T.unpack authUrl)

  -- Start a temporary server to receive the OAuth callback
  codeMVar <- newEmptyMVar :: IO (MVar Text)
  let callbackApp = callbackHandler codeMVar
      settings = Warp.setPort 8085
               $ Warp.setHost "127.0.0.1"
               $ Warp.defaultSettings

  -- Run server in a background thread, wait for code
  _ <- forkIO $ Warp.runSettings settings callbackApp
  code <- takeMVar codeMVar
  TIO.putStrLn "Authorization code received. Exchanging for token..."

  -- Exchange code for token
  tokenResp <- exchangeCode manager clientId clientSecret redirectUri code
  case tokenResp of
    Left err -> TIO.putStrLn $ "Error exchanging code: " <> T.pack err
    Right gtr -> do
      now <- getCurrentTime
      let token = GoogleToken
            { gtAccessToken  = gtrAccessToken gtr
            , gtRefreshToken = maybe "" id (gtrRefreshToken gtr)
            , gtExpiresAt    = addUTCTime (fromIntegral (gtrExpiresIn gtr)) now
            }
      saveToken cfg token
      TIO.putStrLn "Token saved successfully!"

-- | Wai app that handles /callback?code=...
callbackHandler :: MVar Text -> Wai.Application
callbackHandler codeMVar req respond = do
  let path = Wai.pathInfo req
      query = Wai.queryString req
  case path of
    ["callback"] ->
      case lookup "code" query of
        Just (Just codeBS) -> do
          putMVar codeMVar (TE.decodeUtf8 codeBS)
          respond $ Wai.responseLBS
            (toEnum 200)
            [("Content-Type", "text/html")]
            "<html><body><h2>Authorization successful!</h2><p>You can close this window.</p></body></html>"
        _ ->
          respond $ Wai.responseLBS
            (toEnum 400)
            [("Content-Type", "text/plain")]
            "Missing code parameter"
    _ ->
      respond $ Wai.responseLBS
        (toEnum 404)
        [("Content-Type", "text/plain")]
        "Not found"

-- | Exchange authorization code for tokens
exchangeCode :: Manager -> Text -> Text -> Text -> Text -> IO (Either String GoogleTokenResponse)
exchangeCode manager clientId clientSecret redirectUri code = do
  initReq <- parseRequest "https://oauth2.googleapis.com/token"
  let body = TE.encodeUtf8 $ T.intercalate "&"
        [ "code=" <> code
        , "client_id=" <> clientId
        , "client_secret=" <> clientSecret
        , "redirect_uri=" <> redirectUri
        , "grant_type=authorization_code"
        ]
      req = initReq
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/x-www-form-urlencoded")
            ]
        , requestBody = RequestBodyLBS (LBS.fromStrict body)
        }
  resp <- httpLbs req manager
  let status = statusCode (responseStatus resp)
  if status == 200
    then pure $ eitherDecode (responseBody resp)
    else pure $ Left $ "Token exchange error (HTTP " <> show status <> "): "
                     <> show (LBS.toStrict (responseBody resp))

-- | Load token from file, refreshing if expired
loadAndRefreshToken :: GoogleCalendarConfig -> Manager -> IO GoogleToken
loadAndRefreshToken cfg manager = do
  tokenPath <- expandTokenPath (googleTokenPath cfg)
  contents <- LBS.readFile tokenPath
  case eitherDecode contents of
    Left err -> error $ "Failed to parse token file: " <> err
    Right token -> do
      now <- getCurrentTime
      if gtExpiresAt token < addUTCTime 60 now  -- refresh 60s before expiry
        then refreshToken cfg manager token
        else pure token

-- | Refresh an expired token
refreshToken :: GoogleCalendarConfig -> Manager -> GoogleToken -> IO GoogleToken
refreshToken cfg manager token = do
  initReq <- parseRequest "https://oauth2.googleapis.com/token"
  let body = TE.encodeUtf8 $ T.intercalate "&"
        [ "client_id=" <> googleClientId cfg
        , "client_secret=" <> googleClientSecret cfg
        , "refresh_token=" <> gtRefreshToken token
        , "grant_type=refresh_token"
        ]
      req = initReq
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/x-www-form-urlencoded")
            ]
        , requestBody = RequestBodyLBS (LBS.fromStrict body)
        }
  resp <- httpLbs req manager
  let status = statusCode (responseStatus resp)
  if status == 200
    then case eitherDecode (responseBody resp) :: Either String GoogleTokenResponse of
      Left err -> error $ "Failed to parse refresh response: " <> err
      Right gtr -> do
        now <- getCurrentTime
        let newToken = GoogleToken
              { gtAccessToken  = gtrAccessToken gtr
              , gtRefreshToken = maybe (gtRefreshToken token) id (gtrRefreshToken gtr)
              , gtExpiresAt    = addUTCTime (fromIntegral (gtrExpiresIn gtr)) now
              }
        saveToken cfg newToken
        pure newToken
    else error $ "Token refresh error (HTTP " <> show status <> "): "
              <> show (LBS.toStrict (responseBody resp))

-- | Save token to configured path
saveToken :: GoogleCalendarConfig -> GoogleToken -> IO ()
saveToken cfg token = do
  tokenPath <- expandTokenPath (googleTokenPath cfg)
  createDirectoryIfMissing True (takeDirectory tokenPath)
  LBS.writeFile tokenPath (encode token)

-- | Expand ~ in token path to home directory
expandTokenPath :: Text -> IO FilePath
expandTokenPath path = case T.stripPrefix "~/" path of
  Just rest -> do
    home <- getHomeDirectory
    pure $ home </> T.unpack rest
  Nothing -> pure $ T.unpack path
