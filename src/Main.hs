import           Control.Arrow
import           Control.Applicative        (liftA2)
import           Control.Monad.Trans        (lift)
import           Control.Concurrent         (threadDelay)
import qualified Control.Exception          as E
import           Data.Maybe
import           Data.ByteString.Lazy       (ByteString)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.ByteString.Char8      (pack)
import           Network.HTTP.Conduit
import           Network.HTTP.Types         (status200,status303)
import           System.Console.Haskeline
import           System.Environment
import           System.Exit
import           Text.Regex
import           System.Log.Logger
import           System.Log.Handler.Syslog
import           System.Log.Handler.Simple
import           System.Log.Handler         (setFormatter)
import           System.Log.Formatter

getResponse :: String -> IO (Response ByteString)
getResponse url = do
  request' <- parseUrl url
  let request = request' {redirectCount = 0,checkStatus = \_ _ _ -> Nothing}
  withManager $ httpLbs request

isLoggedIn :: IO (Either Bool (Response ByteString))
isLoggedIn = do
  res <- getResponse "http://74.125.236.51:80"
  return $ if responseStatus res /= status303 then Left True else Right res

getMagicString :: String -> Maybe [String]
getMagicString  = matchRegex $ mkRegex "VALUE=\"([0-9a-f]+)\""

getKeepAlive :: String -> Maybe [String]
getKeepAlive    = matchRegex $ mkRegex "location.href=\"(.+?)\""

getLogout :: String -> Maybe [String]
getLogout = matchRegex $ mkRegex "href=\"(.+?logout.+?)\""

keepAlive :: String -> String -> IO ()
keepAlive str logout = E.finally before after      --Logout if exit
  where
    before = do
      infoM "Firewall.keepAlive" "Sending Request to keep Alive"
      _ <- getResponse str
      threadDelay 200000000   -- Wait 200 seconds
      keepAlive str logout
    after = do
      status <- logOut logout
      if status
        then infoM "Firewall.keepAlive" "Logged out successfully"
        else infoM "Firewall.keepAlive" "Cannot logout"

usage :: IO ()
usage   = putStrLn "Version 0.1 beta \nUsage: hwall-auth-iitk [-h] username password"

readInput :: IO (String,String)
readInput = runInputT defaultSettings go
  where
    go :: InputT IO (String,String)
    go = do
      uname <- getInputLine "Username "
      pass <- getPassword Nothing "Password "
      maybe (lift exitFailure) return (liftA2 (,) uname pass)

parse :: [String] -> IO (String,String)
-- parse [] = return ("username","password")
parse ["-h"] = usage >> exitSuccess
parse ["--help"] = usage >> exitSuccess
parse (a:b:_) = return (a,b)
-- Comment this line if your are hardcoding your username and passoword.
parse _ = readInput

getAuthenticationInfo :: IO (String,String)
getAuthenticationInfo = getArgs >>= parse

alreadyLogged :: (String,String) -> Bool -> IO ()
alreadyLogged auth _ =  warningM "Firewall.keepAlive" "Already Logged in .. Trying after 60 seconds " >> threadDelay 60000000 >> firewallAuth auth


logOut :: String -> IO Bool
logOut url = do
  resp <- getResponse url
  return (responseStatus resp == status200 )

tryToLog :: (String,String) -> Response ByteString -> IO ()
tryToLog (username,password) res = do
  infoM "Firewall.login" $ "Hello " ++ username ++ "\nNow trying to login"
  let authLocation = lookup "Location" (read (show $ responseHeaders res) :: [(String,String)])
  authRes <- getResponse (fromJust authLocation) -- Connecting to authentication Location
  let (magicString:_) = fromJust.getMagicString.unpack $ responseBody authRes
  request <- parseUrl (fromJust authLocation)
  resp <- withManager.httpLbs $ urlEncodedBody (map (pack *** pack) [("username",username),("password",password),("magic",magicString),("4Tredir","/")]) request
  let body = responseBody resp
  let (logout:_) = (fromJust.getLogout.unpack $ body)
  infoM "Firewall.login" $ "Logout url is " ++ logout
  let keepAliveMatch = getKeepAlive $ unpack body
  case keepAliveMatch of
    Nothing -> emergencyM "Firewall.login" "Check Username or password" >> exitFailure
    Just (str:_) -> do
      infoM "Firewall.login" $ "Keep Alive URL is " ++ str
      keepAlive str logout

firewallAuth :: (String,String) -> IO ()
firewallAuth auth = do
  infoM "Firewall.keepAlive" "Checking If already Logged in."
  loggedin <- isLoggedIn
  either (alreadyLogged auth) (tryToLog auth) loggedin


retryIfFailed :: IO () -> IO ()
retryIfFailed action = action `catch` with
  where
    with :: HttpException -> IO ()
    with except = do
      errorM "Firewall.keepAlive" $ "Exception: " ++ show except
      errorM "Firewall.keepAlive" "Retrying in 30 seconds"
      threadDelay 30000000
      retryIfFailed action

main :: IO ()
main = do
  auth <- getAuthenticationInfo   -- Getting Username and password
  --   updateGlobalLogger "Firewall.keepAlive" (setLevel INFO)
  updateGlobalLogger "Firewall.login" (setLevel INFO)  
  retryIfFailed (firewallAuth auth) -- Running authentication
