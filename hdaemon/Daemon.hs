import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception hiding (catch)
import Control.Monad
import Network
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Locale
import Char
import Data.Time
import Text.Printf


{-
 
   1. Parse configuration file
   2. Start listening on specified port (default: 3141)                  *DONE*
   3. Accept connections from specified hosts (default: only localhost)  *DONE*
   4. Parse commands (default: start, stop, edit, new)                   *DONE*
   5. Append to log channel                                              *DONE*
   6. Drop connection                                                    *DONE*
   7. Worker thread writes log to a file.                                *DONE*

-}


-- | Editor Commands
--
data Command = CNoParams String 
             | CParams String String

instance Show Command where 
  show (CNoParams s) = toUpper' s
  show (CParams s p) = printf "%s %s" (toUpper' s) p 


defaultCNoParams = ["START", "END"]
defaultCParams   = ["EDIT" , "NEW"]



-- | Program Configuration
--
data Config = Config {
               port :: PortNumber
             , logLocation :: FilePath 
             , verbose :: Bool         -- via command line
             , allowedHosts :: [HostName]
             }
defaultPort = 3141
defaultLogLocation = fmap (</> "editor.log") getHomeDirectory
defaultAllowedHosts = ["localhost"]



-- | Configuration file locations
-- Checks for ~/.elogd and ~/.config/.elogd 

confLocations = mapM (liftM (</> ".elogd")) dirs >>= filterM doesFileExist
  where dirs = [home, liftM (</> ".config") home] 
        home = getHomeDirectory      


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



acceptConnections :: Config -> Socket -> IO ()
acceptConnections conf sock = do 
  (handle, remote, port) <- accept sock
  hSetBuffering handle NoBuffering
  logEvent (remote ++ ": connected.")
  forkIO ( (if elem remote (allowedHosts conf)
             then talk conf handle remote port 
             else logEvent (remote ++ ": hostname rejected."))
          `finally` 
          (logEvent (remote ++ ": closed connection.") 
            >> hClose handle))
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

parseConfiguration :: IO Config
parseConfiguration = do
  args <- getArgs 
  let args' = filter (/="-v") args
      verbose = args' /= args
  when (length args' > 1) usageAndDie
  let log = if null args' then defaultLogLocation else head args
  when ((not . null) args' && head args' == "--help") outputHelp
  if null confLocations 
    then return (Config defaultPort log verbose defaultAllowedHosts)
    else parseConfigurationFile (head confLocations) verbose log

parseConfigurationFile :: FilePath -> Bool -> FilePath -> IO Config
parseConfigurationFile fp verbose log = return (Config defaultPort log verbose defaultAllowedHosts)

usageAndDie = do
  putStrLn usage 
  exitFailure

outputHelp = do 
  putStrLn "elogd - EditTimeReport daemon\n"
  putStrLn usage
  exitFailure

usage = "elogd [-v] [LOGFILE]\n"


main = do conf <- parseConfiguration
          withSocketsDo $ do
          forkIO (eventLogger conf)
          forkIO (commandLogger conf)
          sock <- listenOn (PortNumber $ port conf)
          acceptConnections conf sock `finally` sClose sock


