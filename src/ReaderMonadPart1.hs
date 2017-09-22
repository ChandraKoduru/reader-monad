--Reference web page https://hackernoon.com/the-reader-monad-part-1-1e4d947983a8

module ReaderMonadPart1 where

-- The MonadReader interface solves the problem of threading
-- the same configuration to many functions

-- Imagine this directory

type Config = FilePath

load :: Config -> String -> IO String
load config x = readFile (config ++ x)


loadRevision :: Config -> Int -> IO String
loadRevision config x = load config ("history" ++ show x ++ ".txt")

loadAll :: Config -> Int -> String -> IO (String, String)
loadAll config x y = do
  a <- load config y
  b <- loadRevision config x
  return (a, b)

-- Instead of threading the config to each function, we can write this using
-- MonadReader and configuration get pass implicitly

load1 :: (MonadReader Config m, MonadIO m) => String -> m String
load1 x = do
  config <- ask
  liftIO $ readFile (config ++ x)


loadRevision1 :: (MonadReader Config m, MonadIO m) => Int -> m String
loadRevision1 x = load ("history" ++ show x ++ ".txt")

loadAll1 :: (MonadReader Config m, MonadIO m) => Int -> String -> m (String, String)
loadAll1 x y = do
  a <- load1 y
  b <- loadRevision1 x
  return (a, b)



