{-# Language RankNTypes, QuasiQuotes #-}
module Main (main) where

import Control.Monad
import System.Process           ( createProcess
                                , proc
                                , StdStream( CreatePipe )
                                , CreateProcess ( std_out )
                                , waitForProcess
                                , system
                                )
import System.Exit              ( ExitCode( ExitSuccess ) )
import System.IO                ( hFlush
                                , hPutStr
                                , stdout
                                , withFile
                                , IOMode( WriteMode )
                                )
import System.Environment       ( getArgs )

import System.Console.Docopt ( Docopt
                             , argument
                             , command
                             , docopt
                             , getArg
                             , getArgOrExitWith
                             , isPresent
                             , longOption
                             , parseArgsOrExit
                             )

import qualified Data.Map as M
import Data.Brooks.Vals      ( DVal( StringVal
                                   , IntVal
                                   , MapVal
                                   , TreeVal
                                   , RelVal
                                   )
                             , Tree ( Leaf
                                    , Tree
                                    )
                             )

import IO.Brooks.Database    ( bindName
                             , value
                             )

import IO.Brooks.Timothy     ( newDb
                             , withASE
                             )

import IO.Brooks.Csv         ( relFromCsv )

import Language.Heidi.Lexer  ( alexScanTokens )
--import Language.Heidi.Parser ( parse )


loadRel :: String -> String -> IO ()
loadRel name fn = do
    db <- newDb "test.db"
    putStrLn $ "loadRel! from: " ++ fn
    putStrLn $ "----"
    r <- relFromCsv fn
    putStrLn $ "----"
    putStrLn $ "relFromCsv: " ++ (show r)
    case r of
      Nothing  -> putStrLn $ "Parse error, could not read from: " ++ fn
      Just rel -> do
        withASE db $ \ase -> bindName ase name (RelVal rel)
        putStrLn $ "should have bound name '" ++ name ++ "' to rel val: " ++ (show (RelVal rel))
    -- close db

patterns :: Docopt
patterns = [docopt|
brooksdb

Usage:
  brooksdb get <k>
  brooksdb set <k> <v>
  brooksdb lex <fn>
  brooksdb play <play_arg>
  brooksdb loadrel <name> <file>

Options:
  -h --help     Show this help.
|]

getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs

    -- putStrLn $ "Args: " ++ (show args)

    when (args `isPresent` (command "get")) $ do
      k <- args `getArgOrExit` (argument "k")
      getKey k

    when (args `isPresent` (command "set")) $ do
      k <- args `getArgOrExit` (argument "k")
      v <- args `getArgOrExit` (argument "v")
      setKey k v

    when (args `isPresent` (command "play")) $ do
      playArg <- args `getArgOrExit` (argument "play_arg")
      play playArg

    when (args `isPresent` (command "loadrel")) $ do
      name <- args `getArgOrExit` (argument "name")
      file <- args `getArgOrExit` (argument "file")
      loadRel name file

    when (args `isPresent` (command "lex")) $ do
      fn <- args `getArgOrExit` (argument "fn")
      stream <- readFile fn
      let result = lexMain stream
      putStrLn result

-- --        ["lex", fn] -> do
-- --            stream <- readFile fn
-- --            let result = lexMain stream
-- --            putStrLn result
-- --        ["lex"]     -> do
-- --            stream <- getContents
-- --            let result = lexMain stream
-- --            putStrLn result
-- --        ["parse", fn] -> do
-- --            stream <- readFile fn
-- --            let result = parseMain stream
-- --            putStrLn result
-- --        ["parse"]     -> do
-- --            stream <- getContents
-- --            let result = parseMain stream
-- --            putStrLn result

indent :: String -> String
indent = unlines . map (\s -> "    " ++ s) . lines

play :: String -> IO ()
play arg = do
    db <- newDb "test.db"
    putStrLn "play!"
    putStrLn ("playArg: " ++ arg)
    case arg of
        "get" -> do
            val <- withASE db $ \ase -> value ase "playdb"
            putStrLn $ "The DB: " ++ (show val)
        "set" ->
            withASE db $ \ase -> do let tree = (TreeVal (Tree (Leaf (IntVal 5)) ((Leaf (IntVal 3)))))
                                    --let tree2 = fmap (\x -> x * x) tree
                                    let dbMap = M.insert (StringVal "tree") tree M.empty
                                    bindName ase "playdb" (MapVal dbMap)
                                    putStrLn "Committed."
        _ -> putStrLn "Invalid play arg (try get or set)."


getKey :: String -> IO ()
getKey k = do
    db <- newDb "test.db"
    putStrLn "getKey!"
    val <- withASE db $ \ase -> value ase k
    case val of
        (Just (StringVal s)) -> putStrLn $ "value is: " ++ (show s)
        (Just other) ->
            putStrLn $ "(extraordinary) value is: " ++ (show other)
        Nothing  -> putStrLn $ "no value."
    --close db


setKey :: String -> String -> IO ()
setKey k v = do
    db <- newDb "test.db"
    putStrLn "setKey!"
    withASE db $ \ase -> bindName ase k (StringVal v)
    --close db


lexMain :: String -> String
lexMain stream = show (alexScanTokens stream)

--parseMain :: String -> String
--parseMain stream = show (parse (alexScanTokens stream))
