import Data.Enumerator
import Data.Enumerator.Binary
import Network.HTTP.Enumerator
import System.IO
import System.Posix.Unistd (sleep)
import Control.Concurrent (threadDelay)
import Text.Regex
import Data.Maybe
getResponse url = do
                    request <- parseUrl url 
                    withManager $ \manager -> do
                    httpLbsRedirect request manager
isLoggedIn = do 
            res <- getResponse "http://74.125.67.100:80" 
            putStrLn.show  $ statusCode res
            putStrLn.show $ responseHeaders res
            return (not $ statusCode res == 303)
            --return (responseHeaders res)
getMagicString str = do
                        let regex = mkRegex "VALUE=\"([0-9a-f]+)\""
                        let match = matchRegex regex str
                        match
firewallAuth = do 
                loggedin <- isLoggedIn
                if loggedin then putStrLn "Already Logged in .. Trying after 60 seconds " >> threadDelay 60000000 >> firewallAuth
                    else do 
                            res <- getResponse "http://74.125.67.100:80"
                            let headers = responseHeaders res
                            headers2 <- return.show $  headers
                            putStrLn headers2 
                            --let authLocation = lookup "Location" $ read headers2 :: [(String,String)]
                            --putStrLn.show authLocation

main = firewallAuth 
