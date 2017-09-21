{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char
import Data.List
import Data.Ratio
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as M
import Network
import System.IO
import System.Exit
--import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Random
import Control.Exception
import Text.Printf
import Text.Read

type Triples = [(Maybe T.Text, Maybe T.Text, Maybe T.Text)]
type Chain = M.Map Bigram (M.Map (Maybe T.Text) Integer)
type WeightedList = [(Maybe T.Text, Rational)]
type NormalChain = M.Map Bigram WeightedList

data Bigram = Bigram (Maybe T.Text) (Maybe T.Text) deriving (Eq, Ord)

data Args = Args { corpus :: String
                 , ircnetwork :: String
                 , ircport :: Integer
                 , ircchan :: String
                 , ircnick :: String
                 , ircpassword :: String }

args :: Parser Args
args = Args
    <$> strOption
        ( long "corpus"
       <> short 'c'
       <> value "./marko-chain.corpus"
       <> metavar "PATH"
       <> help "IRC log directory" )
    <*> strOption
        ( long "ircnetwork"
       <> value "irc.snoonet.org"
       <> metavar "IRCNETWORK"
       <> help "IRC network to join" )
    <*> option auto
        ( long "ircport"
       <> value 6667
       <> metavar "IRCPORT"
       <> help "IRC port to connect to" )
    <*> strOption
        ( long "ircchan"
       <> value "#bodyweightfitness"
       <> metavar "IRCCHAN"
       <> help "IRC network to join" )
    <*> strOption
        ( long "ircnick"
       <> value "marko^"
       <> metavar "IRCNICK"
       <> help "IRC nickname to use" )
    <*> strOption
        ( long "ircpassword"
       <> short 'p'
       <> metavar "IRCPASSWORD"
       <> help "IRC password for identification" )

triples :: [T.Text] -> Triples
triples lines = concat $ triples' . T.words <$> lines
  where
    triples' [x] = [(Nothing, Nothing, Just x), (Nothing, Just x, Nothing)]
    triples' xs@(x:(y:_)) =
      (Nothing, Nothing, Just x):((Nothing, Just x, Just y):triples'' xs)
    triples' _ = []

    triples'' (x:(y:(z:zs))) = (Just x, Just y, Just z):triples'' zs
    triples'' [x, y] = [(Just x, Just y, Nothing)]
    triples'' _ = []

buildMap :: Triples -> Chain
buildMap = foldr addOccurence M.empty
  where addOccurence (x, y, z) =
          M.insertWith (M.merge M.preserveMissing M.preserveMissing
                                (M.zipWithMatched (const (+))))
                     (Bigram x y)
                     (M.insert z 1 M.empty)

buildChain :: FilePath -> IO NormalChain
buildChain path = do
  f <- T.readFile path
  let ts = triples (T.lines f)
  return $ (M.toList . ((% 1) <$>)) <$> buildMap ts

generatePhrase :: NormalChain -> Net T.Text
generatePhrase chain = T.intercalate " " <$> generatePhrase' (Bigram Nothing Nothing)
  where
    generatePhrase' :: Bigram -> Net [T.Text]
    generatePhrase' k@(Bigram _ y) =
      case fromList <$> (chain M.!? k) of
        Nothing -> return []
        (Just mz) -> do zMaybe <- mz
                        case zMaybe of
                          Nothing -> return []
                          (Just z) -> do zs <- generatePhrase' (Bigram y zMaybe)
                                         return $ z:zs

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
newtype Bot = Bot { socket :: Handle }

-- Connect to the server and return the initial bot state
connect :: String -> Integer -> IO Bot
connect server port = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h)
  where
    notify = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")

disconnect :: Bot -> IO ()
disconnect = hClose . socket

loop chain nick pass chan = runReaderT (runNet chain nick pass chan)

-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
runNet :: NormalChain -> String -> String -> String -> Net ()
runNet chain nick pass chan = do
    write "NICK" nick
    write "USER" (nick ++ " 0 * :marko-chain")
    asks socket >>= listen chain chan nick pass

-- Process each line from the server
listen :: NormalChain -> String -> String -> String -> Handle -> Net ()
listen chain chan nick pass h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s
      then pong s
      else if auth s
             then privmsg "nickserv" ("identify " ++ pass) >> write "JOIN" chan
             else eval chain chan (clean s)
  where
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)
    auth x    = ":NickServ" `isPrefixOf` x
                && "This nickname is registered" `isPrefixOf` clean x

-- Dispatch a command
eval :: NormalChain -> String -> String -> Net ()
eval _     _        "!quit"                = write "QUIT" ":Exiting"
                                              >> io exitSuccess
eval _     chan x | "!id " `isPrefixOf` x  = privmsg chan (drop 4 x)
eval chain chan x | "!marko" `isPrefixOf` x =
  replicateM_ (fromMaybe 1 (readMaybe (drop 7 x))) $ do
      phrase <- generatePhrase chain
      case phrase of
        t | T.empty == t -> return ()
        phrase -> privmsg chan $ T.unpack phrase
eval _     _        _                      = return () -- ignore everything else

-- Send a privmsg to the current chan + server
privmsg :: String -> String -> Net ()
privmsg target s = write "PRIVMSG" (target ++ " :" ++ s)

-- Send a message out to the server we're currently connected to
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

-- Convenience.
io :: IO a -> Net a
io = liftIO

run :: Args -> IO ()
run as = do
  chain <- buildChain $ corpus as
  let network = ircnetwork as
  let port = ircport as
  let chan = ircchan as
  let nick = ircnick as
  let pass = ircpassword as
  bracket (connect network port) disconnect (loop chain nick pass chan)

main :: IO ()
main = run =<< execParser opts
  where
    versionOption = infoOption "MarkoChain version 1.0" (long "version" <>
                      short 'V' <> help "Show version")
    opts = info (args <**> helper <**> versionOption)
      ( fullDesc
     <> progDesc "Mimick a nick's speech based on irc logs"
     <> header "markochain - a Markov Chain irc bot" )
