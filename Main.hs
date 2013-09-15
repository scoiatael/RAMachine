import Engine
import System.Environment (getArgs, getProgName)
import Data.Char (toUpper)
import Control.Monad (when)
import System.IO (hSetBuffering, BufferMode(NoBuffering),stdin)

main = do 
  args <- getArgs
  (prog, input, eFuncs, flags) <- getEnvir args
  let ms = setInput voidMState input
  let desc = "Exec: " ++ (show prog) ++ "\nAdditional functions: " ++ concatMap (\(ES s,_ ) -> s ++ "\n") eFuncs ++ "\nInput: " ++ show input ++ "\nFlags: " ++ show flags
  when (debugFlag flags) $ putStrLn desc
  runProgram flags (Program prog eFuncs ms) 
  prompt "Again?" main (return ())

getEnvir str = do
  p <- loadF str 0 getProgram
  i <- loadF str 1 getInput
  e <- loadF str 2 getEFuncs
  f <- loadF str 3 getFlags
  return (p,i,e,f)

loadF :: [String] -> Int -> (Maybe String -> a) -> a
loadF s i f = let p = if length s >= i then Just (s !! i) else Nothing in f p

getProgram = getCont readCommands "program"
getInput = getCont readInput "input"
getEFuncs = getCont readEFuncs "extended functions"
getFlags = getCont readFlags "flags"

readCommands str = let l = lines str in map parseCmdLn l
  where
    parseCmdLn str = let w = words str in case length w of
      3 -> (ES $ w !! 0, Prelude.read (concatMap (++" ") (drop 1 w)) :: BCommand)
      2 -> if w !! 1 /= "Halt" then (ES "", Prelude.read (concatMap (++" ") w) :: BCommand) else (ES $ w !! 0, Halt)
      1 -> (ES "", Halt)
readInput str = map (\x -> Prelude.read x :: Integer) $ lines str

{-- TODO --}
readEFuncs str = [] 
{-- TODO --}
readFlags str = Flags 1

getCont readF str ms = do
  fpath <- getfpath str ms 
  file <- readFile fpath
  return $ readF file

getfpath :: String -> (Maybe String) -> IO String 
getfpath str Nothing = do
  putStrLn $ "Load " ++ str ++ " from where?"
  getLine
getfpath str (Just s) = do
  prompt ("Use " ++ s ++ " as " ++ str ++ " path?") (return s) (getfpath str Nothing)  

prompt :: String -> IO a -> IO a -> IO a
prompt s f n = do
  hSetBuffering stdin NoBuffering 
  putStrLn $ s ++ " Y/N"
  s <- getChar
  putStrLn ""
  if ((toUpper s) == 'Y') then f else n
    
