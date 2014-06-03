{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Maybe
import Data.String
import Network.HTTP
import Network.URI
import System.Directory
import System.Environment
import System.FilePath
import qualified Data.ByteString.Lazy as L

openURL :: (HStream s, IsString s) => String -> IO s
openURL x = simpleHTTP (Request (fromJust $ parseURI x) GET [] "")  >>= getResponseBody

potdURL :: String
potdURL = "http://www.bbc.co.uk/podcasts/series/podcastoftheday"

potdPrefix :: String
potdPrefix = "http://downloads.bbc.co.uk/podcasts/worldservice/podcastoftheday/podcastoftheday_"

findPodcasts :: String -> [String]
findPodcasts [] = []
findPodcasts s = case stripPrefix potdPrefix s of 
    (Just s') -> let (ident, rest) = helper s' in ident : findPodcasts rest 
    Nothing    -> findPodcasts $ tail s 
    where helper [] = error "unexpected end of link name in function findPocasts"
          helper ('.':'m':'p':'3':s'') = ([], s'')
          helper (c:s'') = let (ident, rest) = helper s'' in (c:ident, rest)
          
toURL :: String -> String
toURL ident = podcastPrefix ++ ident ++ ".mp3"

toFilename :: String -> String
toFilename ident = let (year,   rest)  = splitAt 4 ident
                       (month, rest')  = splitAt 2 rest
                       (day      , _)  = splitAt 2 rest'
                   in concat $ intersperse "_" ["新闻播客", day, month, year ++ ".mp3"]
          
filterNew :: FilePath -> [String] -> IO [String]
filterNew directory idents = do createDirectoryIfMissing True directory
                                old <- getDirectoryContents directory
                                return $ filter (not . (`elem` old) . toFilename) idents
              
allPodcastIdents :: IO [String]
allPodcastIdents = fmap findPodcasts $ openURL potdURL

slash :: String -> String -> String
directory `slash` filename = let directory' = helper directory last init
                                 filename'  = helper filename head tail
                             in directory' ++ pathSeparator : filename'
    where helper s f g = if f s == pathSeparator then g s else s

main :: IO ()
main = do [directory] <- getArgs
          idents      <- allPodcastIdents >>= filterNew directory
          bodies <- mapM (openURL . toURL) idents
          mapM_ (\(ident, body) -> L.writeFile (directory `slash` toFilename ident) body) $ zip idents bodies
          

