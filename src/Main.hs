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
--import System.Console.Readline ( readline )

import qualified Data.Map as M
import Data.Brooks.Vals      ( DVal( StringVal
                                   , IntVal
                                   , MapVal
                                   , TreeVal
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

--import Language.Heidi.Lexer  ( alexScanTokens )
--import Language.Heidi.Parser ( parse )


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
        ["play", playArg]      -> play playArg
        ["repl"] -> repl
--        _      -> putStrLn "<get k>, <set k v>, <lex [fn]>, <parse [fn]>, <play>, <repl>"
        _      -> putStrLn "<get k>, <set k v>, <play>, <repl>"
    putStrLn "Done"

indent :: String -> String
indent = unlines . map (\s -> "    " ++ s) . lines

executeExpr :: String -> IO ()
executeExpr expr = do
    let makeCmd = [ "--make"
                  , "-fbuilding-cabal-package"
                  , "-O"
                  , "-odir"
                  , "dist/build/brooksdb/brooksdb-tmp"
                  , "-hidir"
                  , "dist/build/brooksdb/brooksdb-tmp"
                  , "-stubdir"
                  , "dist/build/brooksdb/brooksdb-tmp"
                  , "-i"
                  , "-idist/build/brooksdb/brooksdb-tmp"
                  , "-isrc"
                  , "-idist/build/autogen"
                  , "-Idist/build/autogen"
                  , "-Idist/build/brooksdb/brooksdb-tmp"
                  , "-optP-include"
                  , "-optPdist/build/autogen/cabal_macros.h"
                  , "-hide-all-packages"
                  , "-no-user-package-conf"
                  , "-package-conf"
                  , "/Users/john/src/brooksdb/cabal-dev/packages-7.4.2.conf"
                  , "-package-conf"
                  , "dist/package.conf.inplace"
                  , "-package-id"
                  , "MissingH-1.2.0.0-95523ba02ec785a49f27b142c972a719"
                  , "-package-id"
                  , "acid-state-0.8.3-be154b18bc6173ec120110b3bd03d32e"
                  , "-package-id"
                  , "array-0.4.0.0-0b6c5ca7e879a14d110ca4c001dd9297"
                  , "-package-id"
                  , "base-4.5.1.0-47f48c3ae7f8256a66a23e9dfe22eefc"
                  , "-package-id"
                  , "containers-0.4.2.1-75f143aa39a3e77a1ce2300025bdd8ce"
                  , "-package-id"
                  , "mtl-2.1.2-02e701f9b1590ee88a0b5b0bd5d93a29"
                  , "-package-id"
                  , "plugins-1.5.3.0-666abe82b271d247926c7b7c175e3bc6"
                  , "-package-id"
                  , "process-1.1.0.1-f002477a35b880981ab3a429893cfea4"
                  , "-package-id"
                  , "random-1.0.1.1-bacb9d7d3919803c7a5c2c58c533c2b7"
                  , "-package-id"
                  , "safecopy-0.8.2-434a445bf80b26dbdf8d98fc7ec05bc1"
                  , "-XHaskell2010"
                  , "src/CurQuery.hs"
                  , "-o"
                  , "dist/build/brooksdb/curquery"
                  , "-Wall"
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
    forever $ do
        putStr ":: "
        hFlush stdout
        expr <- getLine
        putStrLn ("expr:(" ++ expr ++ ")")
        executeExpr expr

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
