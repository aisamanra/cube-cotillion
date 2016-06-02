module Network.CubeCotillion where

import           Control.Concurrent (forkIO)
import           Control.Monad.IO.Class(MonadIO(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Char8 (pack, unpack)
import           GHC.Exts (IsString(..))
import           MonadLib
import           Network
import           Network.SSH.Server

type Key = [ServerCredential]

loadKey = loadPrivateKeys

cubeCotillion :: Int -> Key -> CubeM () -> IO ()
cubeCotillion port keys cube = withSocketsDo $ do
  let routes = getRoutes cube
  sock <- listenOn (PortNumber (fromIntegral port))
  let server = Server
        { sAccept             = mkHandlers sock routes
        , sAuthenticationAlgs = keys
        , sVersion            = "CubeCotillion_0.0"
        , sDebugLevel         = 0
        }
  sshServer server

mkHandlers :: Socket -> [Route] -> IO (SessionHandlers, HandleLike)
mkHandlers sock routes = do
  (handle, _, _) <- accept sock
  let handlers = SessionHandlers
        { cOpenShell = \ _ _ _ _ _ -> return False
        , cDirectTcp = \ _ _ _ _ -> return False
        , cRequestSubsystem = \ _ _ _ -> return False
        , cAuthHandler = \ _ _ _ _ -> return AuthAccepted
        , cRequestExec = \ cmd _ write -> do
              _ <- forkIO $ do
                case dispatchRoutes routes cmd of
                  Just action -> action write >> write Nothing
                  Nothing     -> write Nothing
              return True
        }
  return (handlers, handle2HandleLike handle)

cmd :: CommandPattern -> ActionM () -> CubeM ()
cmd command action = CubeM (put [Route command action])

bs :: ByteString -> ActionM ()
bs val = do
  (_, write) <- ActionM ask
  ActionM $ inBase $ write (Just val)

string :: String -> ActionM ()
string str = do
  (_, write) <- ActionM ask
  ActionM $ inBase $ write (Just (pack str))

param :: ByteString -> ActionM ByteString
param name = do
  (vars, _) <- ActionM ask
  let Just val = lookup name vars
  return val

readParam :: Read a => ByteString -> ActionM a
readParam name = do
  r <- param name
  return (read (unpack r))

data Fragment
  = Word ByteString
  | Var ByteString
    deriving (Eq, Show)

newtype CommandPattern = CommandPattern
  { commandChunks :: [Fragment] } deriving (Eq, Show)

instance IsString CommandPattern where
  fromString str =
    CommandPattern
      [ if BS.head c == 58
          then Var (BS.tail c)
          else Word c
      | c <- BS.split 32 (pack str)
      ]

match :: ByteString -> CommandPattern -> Maybe [(ByteString, ByteString)]
match bs cmd = go (BS.split 32 bs) (commandChunks cmd)
  where go [] [] = Just []
        go (b:bs) (Var t:ts) =
          ((t, b):) `fmap` go bs ts
        go (b:bs) (Word t:ts)
          | b == t    = go bs ts
          | otherwise = Nothing
        go _ _ = Nothing

data Route = Route
  { routePattern :: CommandPattern
  , routeAction  :: ActionM ()
  }

getRoutes :: CubeM () -> [Route]
getRoutes = snd . runId . runWriterT . runCubeM

dispatchRoutes :: [Route] -> ByteString -> Maybe (Writer -> IO ())
dispatchRoutes [] _ = Nothing
dispatchRoutes (r:rs) bs =
  case match bs (routePattern r) of
    Nothing -> dispatchRoutes rs bs
    Just vs -> Just (runActionWith vs (routeAction r))

newtype CubeM a = CubeM
  { runCubeM :: WriterT [Route] Id a }

instance Functor CubeM where
  fmap f (CubeM x) = CubeM (fmap f x)

instance Applicative CubeM where
  pure x = CubeM (pure x)
  CubeM f <*> CubeM x = CubeM (f <*> x)

instance Monad CubeM where
  CubeM x >>= f = CubeM (x >>= runCubeM . f)

type Env = [(ByteString, ByteString)]
type Writer = Maybe ByteString -> IO ()

newtype ActionM a = ActionM
  { runActionM :: ReaderT (Env, Writer) IO a }

runActionWith :: Env -> ActionM () -> Writer -> IO ()
runActionWith env action writer =
  runReaderT (env, writer) (runActionM action)

instance Functor ActionM where
  fmap f (ActionM x) = ActionM (fmap f x)

instance Applicative ActionM where
  pure x = ActionM (pure x)
  ActionM f <*> ActionM x = ActionM (f <*> x)

instance Monad ActionM where
  ActionM x >>= f = ActionM (x >>= runActionM . f)

instance MonadIO ActionM where
  liftIO mote = ActionM (inBase mote)
