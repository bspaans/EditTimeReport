import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception hiding (catch)
import Control.Monad
import Network
import System.IO
import System.IO.Unsafe
import System.Locale
import Char
import Data.Time
import Text.Printf
import Prelude hiding (log)


{-
 
   1. Parse configuration file
   2. Start listening on specified port (default: 3141)
   3. Accept connections from specified hosts (default: only localhost)
   4. Parse commands (default: start, stop, edit, new)
   5. Append to log channel
   6. Drop connection
   7. Worker thread writes log to a file.

-}

data Command = CNoParams String 
             | CParams String String

instance Show Command where 
  show (CNoParams s) = toUpper' s
  show (CParams s p) = printf "%s %s" (toUpper' s) p 


defaultCNoParams = ["START", "END"]
defaultCParams   = ["EDIT" , "NEW"]


-- There are two global channels:
-- One for program events (eg. who connected, what was entered, etc.)
-- and one for the results.

{-# NOINLINE eventChan #-}
eventChan :: Chan String
eventChan = unsafePerformIO newChan

{-# NOINLINE commandChan #-}
commandChan :: Chan Command
commandChan = unsafePerformIO newChan


logEvent :: String -> IO ()
logEvent = writeChan eventChan

logCommand :: Command -> IO ()
logCommand = writeChan commandChan

data Config = Config {
               port :: PortNumber
             , logLocation :: FilePath
             , verbose :: Bool
             }

defaultConfig = Config 3141 "test.log" False


acceptConnections :: Config -> Socket -> IO ()
acceptConnections conf sock = do 
  (handle, remote, port) <- accept sock
  hSetBuffering handle NoBuffering
  logEvent (remote ++ ": connected.")
  forkIO (talk conf handle remote port `finally` hClose handle)
  acceptConnections conf sock


talk :: Config -> Handle -> HostName -> PortNumber -> IO()
talk conf handle remote port = do 
  line <-fmap (filter (/= '\r')) (hGetLine handle)
  logEvent (remote ++ ": entered command: `" ++ line ++ "' (" ++ show (length line) ++ ").")
  case parseLine conf line of
    Just s -> logCommand s >> logEvent (remote ++ ": succesfully parsed command.") >> talk conf handle remote port
    Nothing -> do logEvent (remote ++ ": unable to parse command.")
                  if toUpper' line == "QUIT" 
                    then return()
                    else talk conf handle remote port


parseLine :: Config -> String -> Maybe Command
parseLine conf s = case words s of 
 [] -> Nothing
 (x:xs) -> if elem (toUpper' x) defaultCNoParams
             then Just (CNoParams x)
             else if elem (toUpper' x) defaultCParams
                     && (not . null $ xs)
                     then Just (CParams x  (unwords xs))
                     else Nothing

toUpper' = map toUpper

type WorkerThread = IO ()

eventLogger :: Config -> WorkerThread
eventLogger conf = do
  str <- readChan eventChan
  time <- getZonedTime
  when (verbose conf) ( do
    let t = formatTime defaultTimeLocale "%c" time
    printf "%s %s\n" t str
    )
  eventLogger conf


commandLogger :: Config -> WorkerThread
commandLogger conf = do
  cmd <- readChan commandChan 
  time <- getZonedTime
  let t = formatTime defaultTimeLocale "%Y %m %d %H %M %S %w %j" time
  appendFile (logLocation conf) (printf "%s %s\n" t (show cmd))
  commandLogger conf


mainLoop :: Config -> IO ()
mainLoop conf = withSocketsDo $ do
  forkIO (eventLogger conf)
  forkIO (commandLogger conf)
  sock <- listenOn (PortNumber $ port conf)
  acceptConnections conf sock


main = mainLoop defaultConfig
