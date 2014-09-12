{-#Language RankNTypes #-}
module Main (main) where

import Control.Monad
import System.Process        ( createProcess
                             , proc
                             , StdStream( CreatePipe )
                             , CreateProcess ( std_out )
                             , waitForProcess
                             , system
                             )
import System.Exit           ( ExitCode( ExitSuccess ) )
import System.IO             ( hFlush
                             , hPutStr
                             , stdout
                             , withFile
                             , IOMode( WriteMode )
                             )
import System.Environment    ( getArgs )
import System.Console.Readline ( readline
                               , addHistory
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

--import Language.Heidi.Lexer  ( alexScanTokens )
--import Language.Heidi.Parser ( parse )


loadRel :: String -> String -> IO ()
loadRel name fn = do
    db <- newDb "test.db"
    putStrLn $ "yarp! loadRel! from: " ++ fn
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


main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "Args: " ++ (show args)
    case args of
        ["get", k]    -> getKey k
        ["set", k, v] -> setKey k v
--        ["lex", fn] -> do
--            stream <- readFile fn
--            let result = lexMain stream
--            putStrLn result
--        ["lex"]     -> do
--            stream <- getContents
--            let result = lexMain stream
--            putStrLn result
--        ["parse", fn] -> do
--            stream <- readFile fn
--            let result = parseMain stream
--            putStrLn result
--        ["parse"]     -> do
--            stream <- getContents
--            let result = parseMain stream
--            putStrLn result
        ["play", playArg]        -> play playArg
        ["loadrel", name, file]  -> loadRel name file
        ["repl"] -> repl
--        _      -> putStrLn "<get k>, <set k v>, <lex [fn]>, <parse [fn]>, <play>, <repl>"
        _      -> putStrLn "<get k>, <set k v>, <loadrel>, <play>, <repl>"
    putStrLn "Done"

indent :: String -> String
indent = unlines . map (\s -> "    " ++ s) . lines

executeExpr :: String -> IO ()
executeExpr expr = do
    let makeCmd = [ "--make"
                  , "-fbuilding-cabal-package"
                  , "-O"
                  , "-odir", "dist/build/brooksdb/brooksdb-tmp"
                  , "-hidir" , "dist/build/brooksdb/brooksdb-tmp"
                  , "-stubdir", "dist/build/brooksdb/brooksdb-tmp"
                  , "-i"
                  , "-idist/build/brooksdb/brooksdb-tmp"
                  , "-isrc"
                  , "-idist/build/autogen"
                  , "-Idist/build/autogen"
                  , "-Idist/build/brooksdb/brooksdb-tmp"
                  , "-optP-include"
                  , "-optPdist/build/autogen/cabal_macros.h"
                  , "-hide-all-packages"
                  , "-no-user-package-db"
                  , "-package-db" , "./.cabal-sandbox/x86_64-osx-ghc-7.6.3-packages.conf.d"
                  , "-package-db" , "dist/package.conf.inplace"
                  , "-package", "MissingH-1.2.0.0"
                  , "-package", "acid-state"
                  , "-package", "array-0.4.0.1"
                  , "-package", "base-4.6.0.1"
                  , "-package", "containers-0.5.0.0"
                  , "-package", "mtl-2.1.2"
                  , "-package", "plugins-1.5.3.0"
                  , "-package", "process-1.1.0.2"
                  , "-package", "random-1.0.1.1"
                  , "-package", "safecopy-0.8.2"
                  , "-XHaskell2010"
                  , "src/CurQuery.hs"
                  , "-o"
                  , "dist/build/brooksdb/curquery"
--                  , "-Wall"
--                  , "-v"
                  ]
    prescript <- readFile "pre.txt"
    postscript <- readFile "post.txt"
    let chromed = unlines [ prescript
                          , indent expr
                          , indent postscript
                          ]
    withFile "src/CurQuery.hs" WriteMode $ \fh -> hPutStr fh chromed
    (_, Just _hout, _, procHdl) <- createProcess (proc "ghc" makeCmd){ std_out = CreatePipe }
    exitCode <- waitForProcess procHdl
    putStrLn $ "GHC exitCode: " ++ (show exitCode)
    case exitCode of
        ExitSuccess -> do
            queryExitCode <- system "./dist/build/brooksdb/curquery"
            putStrLn $ "queryExitCode: " ++ (show queryExitCode)

        _ -> putStrLn "Query not executed (failed to compile)."
    -- execute it
    -- print the whole db (for now)

repl :: IO ()
repl = do
    putStrLn "Welcome to brooksdb."
    doRepl

doRepl :: IO ()
doRepl = do
    maybeLine <- readline ":: "
    case maybeLine of
         Nothing ->     return ()  -- EOF / control-d
         Just "exit" -> return ()
         Just "quit" -> return ()
         Just "q."   -> return ()
         Just expr   -> do addHistory expr
                           putStrLn ("expr:(" ++ expr ++ ")")
                           executeExpr expr
                           doRepl

play :: String -> IO ()
play arg = do
    db <- newDb "test.db"
    putStrLn "yarp! play!"
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
    putStrLn "yarp! getKey!"
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
    putStrLn "yarp! setKey!"
    withASE db $ \ase -> bindName ase k (StringVal v)
    --close db


--lexMain :: String -> String
--lexMain stream = show (alexScanTokens stream)

--parseMain :: String -> String
--parseMain stream = show (parse (alexScanTokens stream))
