import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception hiding (catch)
import Network
import System.IO
import System.IO.Unsafe
import Char
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

defaultCNoParams = ["START", "END"]
defaultCParams   = ["EDIT" , "NEW"]

{-# NOINLINE logChannel #-}
logChannel :: Chan String
logChannel = unsafePerformIO newChan

log :: String -> IO ()
log = writeChan logChannel 


data Config = C

acceptConnections :: Config -> Socket -> IO ()
acceptConnections conf sock = do 
  (handle, remote, port) <- accept sock
  forkIO (talk conf handle remote port `finally` hClose handle)
  acceptConnections conf sock


talk :: Config -> Handle -> HostName -> PortNumber -> IO()
talk conf handle remote port = do 
  hSetBuffering handle NoBuffering
  log $ remote ++ " connected."
  hPutStrLn handle $ "Hi there " ++ remote ++ "!"
  line <- hGetLine handle
  log $ remote ++ " entered " ++ line
  case parseLine conf line of
    Just s -> hPutStrLn handle "Valid command" >> log "parsed"
    Nothing -> hPutStrLn handle "Invalid command" >> log "no parse"


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

logWorkerThread :: IO()
logWorkerThread = do
  str <- readChan logChannel 
  putStrLn str
  logWorkerThread

mainLoop :: PortNumber -> IO ()
mainLoop port = withSocketsDo $ do
  forkIO logWorkerThread
  sock <- listenOn (PortNumber port)
  acceptConnections C sock
