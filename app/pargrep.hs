module Main where

import Control.Monad
import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

{-
main = do
    (args, files) <- getArgs >>= parse
    mapM_ (pargrep args) files
-}
withFile s f = putStr . unlines . f . lines =<< open s
    where
    open f = if f == "-" then getContents else readFile f

pargrep [] f = Main.withFile f id

{- Build our own flag datatype -}
data Flag
    = Parallel              -- -p
    | PatternFile           -- -f 
    | Extended              -- -Ee
    | Number                -- -n
    | Help                  -- --help
    deriving (Eq,Ord,Enum,Show,Bounded)

{- Specify our flags to be used for this program -}
flags =
    [Option ['p'] ["parallel"]              (NoArg Parallel)
        "Enables parallel processing of files."
    ,Option ['f'] ["input-file"]            (NoArg PatternFile)
        "Use search patterns within a newline delimited file."
    ,Option ['E', 'e'] ["extended-regexp"]  (NoArg Extended)
        "Interpret pattern as an extended regular expression."
    ,Option ['n'] ["line-numbers"]          (NoArg Number)
        "Each output line is preceded by its relative line number in the file, starting at line 1."
    ,Option []    ["help"] (NoArg Help)
        "Print this help message"
    ]

{- Parse Command-line Arguments -}
parse argv = case getOpt Permute flags argv of
    ([], [], _) -> do 
        hPutStrLn stderr (usageInfo header flags) 
        exitSuccess
    (args, fs,[]) -> do
        let files = if null fs then ["-"] else fs
        --Exit if help is specified
        if Help `elem` args
            then do hPutStrLn stderr (usageInfo header flags)
                    exitSuccess
        else if PatternFile `elem` args
                then do 
                    content <- readFile (head files)
                    let patterns = lines content
                    return (nub (concatMap set args), patterns, tail files)
             else return (nub (concatMap set args), [head files], tail files)

    (_,_, errs)      -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)

    where header = "Usage: pargrep [-pEn] [pattern] [file ...]"
          set f  = [f]

{- Actually process the arguments -}
main :: IO ()
main = do
    (as, pn, fs) <- getArgs >>= parse
    
    putStrLn $ "Flags: " ++ show as
    putStrLn $ "Pattern: "++ show pn
    putStrLn $ "Files: " ++ show fs
   
    content <- readFile $ head fs
    print content


