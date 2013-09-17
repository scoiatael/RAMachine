import Engine
import System.Environment (getArgs, getProgName)
import Data.Char (toUpper)
import Control.Monad (when)
import System.IO (hSetBuffering, BufferMode(NoBuffering),stdin)

main = do 
  args <- getArgs
  (prog, input, eFuncs, flags) <- getEnvir args
  let ms = setInput voidMState input
  let desc = "Exec: " ++ (show prog) ++ (if not $ empty eFuncs then "\nAdditional functions: " ++ concatMap (\(ES s,_ ) -> s ++ "\n") eFuncs else "") ++ (if not $ empty input then "\nInput: " ++ show input else "") ++ (if flags /= Flags 0 then "\nFlags: " ++ show flags else "")
  when (True || debugFlag flags) $ putStrLn desc
  runProgram flags (Program prog eFuncs ms) 
  prompt "Again?" main (return ())
  where 
  empty a = length a == 0

getEnvir str = do
  p <- loadF str 0 getProgram
  i <- loadF str 1 getInput
  f <- loadF str 2 getFlags
  e <- loadF str 3 getEFuncs
  return (p,i,e,f)

loadF :: [String] -> Int -> (Maybe String -> a) -> a
loadF s i f = let p = if length s > i then Just (s ^! (toInteger i)) else Nothing in f p

getProgram = getCont readCommands "program"
getInput = getCont' readInput "input"
getEFuncs = getCont' readEFuncs "extended functions"
getFlags = getCont' readFlags "flags"

readCommands str = let l = lines str in map parseCmdLn l
  where
    parseCmdLn str = let w = words str in case length w of
      3 -> (ES $ w ^! 0, Prelude.read (concatMap (++" ") (drop 1 w)) :: BCommand)
      2 -> if w ^! 1 /= "Halt" then (ES "", Prelude.read (concatMap (++" ") w) :: BCommand) else (ES $ w ^! 0, Halt)
      1 -> (ES "", Halt)

readInput :: String -> [Integer] 
readInput str = map (\x -> Prelude.read x :: Integer) $ lines str

{-- TODO --}
readEFuncs :: String -> [(EString, EInt -> Command ())]
readEFuncs str = [] 

readFlags :: String -> Flags
readFlags str = Flags $ foldl (\a b -> a + 2^b) 0 $ map findFlagN $ map (filter (not. (flip elem) whitespace)) $ lines str
  where
    findFlagN s = head $ concatMap (\(ES a,b) -> if a == s then [b] else [] ) $ zip flags [0..]::Int

getCont readF str ms = do
  fpath <- getfpath str ms 
  file <- readFile fpath
  return $ readF file

getCont' readF str ms = do
  fpath <- getfpath' str ms 
  if fpath /= ""  then do { file <- readFile fpath; return $ readF file; } else do { return $ readF ""; }

getfpath' :: String -> (Maybe String) -> IO String 
getfpath' str (Just s) = getfpath str (Just s)
getfpath' str Nothing = prompt ("Use " ++ str ++ " ?") (getfpath str Nothing) (return "")

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
    
