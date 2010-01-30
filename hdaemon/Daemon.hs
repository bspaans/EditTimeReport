import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception hiding (catch)
import Control.Monad
import Data.Char
import Data.Time
import Network
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Locale
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


-- | Program Configuration
--
data Config = Config {
               port :: PortNumber
             , logLocation :: FilePath 
             , verbose :: Bool         -- via command line
             , allowedHosts :: [HostName]
             }
defaultPort :: PortNumber
defaultPort = 3141

defaultLogLocation :: IO FilePath
defaultLogLocation = fmap (</> "editor.log") getHomeDirectory

defaultAllowedHosts :: [HostName]
defaultAllowedHosts = ["localhost"]



-- | Editor Commands.
-- Commands have either no arguments (ie. start, end)
-- or one (edit, new).
--
data Command = CNoParams String 
             | CParams String String

instance Show Command where 
  show (CNoParams s) = toUpper' s
  show (CParams s p) = printf "%s %s" (toUpper' s) p 


defaultCNoParams = ["START", "END"]
defaultCParams   = ["EDIT" , "NEW"]




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
  logEvent (remote ++ ": entered command: `" ++ line ++ "'.")
  case parseCommand line of
    Just s  -> do logCommand s 
                  logEvent (remote ++ ": succesfully parsed command.") 
                  talk conf handle remote port
    Nothing -> do logEvent (remote ++ ": unable to parse command.")
                  if toUpper' line == "QUIT" 
                    then return()
                    else talk conf handle remote port


parseCommand :: String -> Maybe Command
parseCommand s = case words s of 
 [] -> Nothing
 (x:xs) -> if elem (toUpper' x) defaultCNoParams
             then Just (CNoParams x)
             else if elem (toUpper' x) defaultCParams
                     && (not . null $ xs)
                     then Just (CParams x  (unwords xs))
                     else Nothing

toUpper' :: String -> String
toUpper' = map toUpper


-- | Worker threads
--
type WorkerThread = IO ()

-- | Logs events to stdout if verbose flag is set
--
eventLogger :: Config -> WorkerThread
eventLogger conf = do
  str <- readChan eventChan
  time <- getZonedTime
  when (verbose conf) ( do
    let t = formatTime defaultTimeLocale "%c" time
    printf "%s %s\n" t str
    )
  eventLogger conf

-- | Logs commands to specified file
-- 
commandLogger :: Config -> WorkerThread
commandLogger conf = do
  cmd <- readChan commandChan 
  time <- getZonedTime
  let t = formatTime defaultTimeLocale "%Y %m %d %H %M %S %w %j" time
  appendFile (logLocation conf) (printf "%s %s\n" t (show cmd))
  commandLogger conf


-- | Configuration parser.
-- Gathers options from command line arguments
-- and configuration files.
--
parseConfiguration :: IO Config
parseConfiguration = do
  args <- getArgs 
  let args' = filter (/="-v") args
      verbose = args' /= args
  when (length args' > 1) usageAndDie
  log <- if null args' then defaultLogLocation else return $ head args
  when ((not . null) args' && head args' == "--help") outputHelp
  clocs <- confLocations
  if null clocs
    then return (Config defaultPort log verbose defaultAllowedHosts)
    else parseConfigurationFile (head clocs) verbose log

-- | TODO: parse configuration file
--
parseConfigurationFile :: FilePath -> Bool -> FilePath -> IO Config
parseConfigurationFile fp verbose log = return (Config defaultPort log verbose defaultAllowedHosts)


-- | Program
--
usageAndDie = do
  putStrLn usage 
  exitFailure

outputHelp = do 
  putStrLn "elogd - EditTimeReport daemon\n"
  putStrLn usage
  exitFailure

usage :: String
usage = "elogd [-v] [LOGFILE]\n"


main :: IO ()
main = do conf <- parseConfiguration
          withSocketsDo $ do
          forkIO (eventLogger conf)
          forkIO (commandLogger conf)
          sock <- listenOn (PortNumber $ port conf)
          acceptConnections conf sock `finally` sClose sock


