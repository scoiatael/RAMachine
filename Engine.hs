{-# LANGUAGE DeriveDataTypeable #-}

module Engine where
import Prelude hiding (read, Read)
import qualified Prelude (read, Read)
import Data.Bits (shiftR, (.&.))
import Control.Exception (Exception, throw, catch)
import Control.Monad (when)
import Data.Typeable (Typeable)

data MState = MState { registers :: [Integer], accumulator :: Integer, curInstr :: Integer, input :: [Integer], output :: [Integer] }
voidMState = MState [0,0..] 0 0 [] []
setInput ms i = ms { input = i }

instance Show MState where
  show m = "Output: " ++ (show $ output m)

data Command a = Command { runC::MState -> (a, MState) }

instance Monad Command where
  return a = Command $ \ms -> (a, ms)
  g >>= f = Command $ \ms -> let (a, ms') = runC g $ ms in runC (f a) $ ms'

(^!) :: [a] -> Integer -> a
t ^! i = if i < 0 then throw NegativeIndex else t !! (toEnum (fromEnum i) :: Int)
modTable :: [a] -> Integer -> a -> [a]
modTable t i a = let (h, (_:t')) = splitAt (toEnum (fromEnum i) :: Int) t in h ++ (a:t')

data EInt = Num Integer | Ind Integer | Dir Integer deriving (Eq)
instance Prelude.Read EInt where
  readsPrec = readEInt

readEInt d i = readNum i ++ readInd i ++ readDir i
  where
    readDir i = do
      (v,i') <- readsPrec d i
      return (Dir v,i')
    readInd i = do
      ("^", s) <- lex i
      (v, i') <- readsPrec d s
      return (Ind v, i')
    readNum i = do
      ("=", s) <- lex i
      (v, i') <- readsPrec d s
      return (Num v, i')
    
instance Show EInt where
  show (Num i) = "=" ++ (show i)
  show (Ind i) = "^" ++ (show i)
  show (Dir i) = (show i)

fromEInt :: MState -> EInt -> Integer
fromEInt ms (Num i) = i
fromEInt ms (Ind i) = let r = registers ms in r ^! (r ^! i)
fromEInt ms (Dir i) = registers ms ^! i

fromEInt' ms (Num _) = throw IndexNotApplicable
fromEInt' ms (Dir i) = i
fromEInt' ms (Ind i) = registers ms ^! i

findLabel :: [(EString,a)] -> EString -> Integer
findLabel cmds s = let nrs = filter (\((s',_),i) -> s==s') $ zip cmds [0..] in
  if (length nrs == 0) then throw NoSuchLabel
  else if (length nrs > 1) then throw AmbiguousLabel else snd $ nrs !! 0
wrapC f = Command $ \m -> ((), f m)
readAccum = Command $ \ms -> (accumulator ms, ms)
writeAccum i = wrapC $ \ms -> ms { accumulator = i }
pushInsNum = wrapC $ \ms -> ms {curInstr = accumulator ms}
withNoAccumMod :: Command () -> Command ()
withNoAccumMod f = do
  a <- readAccum
  f
  writeAccum a
nextIns = wrapC $ \ms -> ms { curInstr = curInstr ms + 1 }
modAccum f = wrapC $ \ms -> let accum = accumulator ms in ms { accumulator = f accum }
--
load :: EInt -> Command ()
load e = wrapC $ \ms -> ms { accumulator = fromEInt ms e}
store :: EInt -> Command ()
store e = wrapC $ \ms -> ms { registers = modTable (registers ms) (fromEInt' ms e) (accumulator ms) } 
write :: Command ()
write = wrapC $ \ms -> let accum = accumulator ms; op = output ms in ms {output = accum:op }
read :: Command ()
read = wrapC $ \ms -> let (c:r) = input ms in ms { accumulator = c, input = r }
jump :: Program -> EString -> Command ()
jump p s = do
  let cmds = commands p
  let nr = findLabel cmds s
  withNoAccumMod $ do { writeAccum nr; pushInsNum; }

   

data BCommand =  Halt | Load EInt | Store EInt 
                  | Add EInt | Mult EInt | Div EInt | Sub EInt
                  | Read EInt | Write EInt 
                  | Jump EString | Jgtz EString | Jzero EString
                  | Ext EString EInt 
                  deriving (Eq, Show,Prelude.Read)

data EString = ES String deriving (Eq)
instance Prelude.Read EString where
  readsPrec d r = [(ES $ (filter (not . (flip elem) whitespace)) r, [])]
whitespace = " \n\r"
instance Show EString where
  show (ES s) = show s

data RAMException = WrongLabel | Done | DivisionByZero 
  | NegativeIndex | IndexNotApplicable 
  | NoSuchLabel | AmbiguousLabel
  deriving (Typeable, Show)
instance Exception RAMException
runCom :: Program -> (EString,BCommand) -> Command ()
runCom p (_, Halt) = throw Done
runCom p (_, Jump s) = jump p s
runCom p (_, Jgtz s) = jf (>=0) p s
runCom p (_, Jzero s) = jf (==0) p s
runCom p (_, Load e) = (load e) >> nextIns
runCom p (_, Store e) = (store e) >> nextIns
runCom p (_, Read e) = (withNoAccumMod $ do { read; store e; }) >> nextIns
runCom p (_, Write e) = (withNoAccumMod $ do { load e; write; })  >> nextIns
runCom p (_, Add e) = (accumf (+) e) >> nextIns
runCom p (_, Mult e) = (accumf (*) e) >> nextIns
runCom p (_, Sub e) = (accumf (-) e) >> nextIns
runCom p (_, Div e) = (accumf (\a b -> if b ==0 then throw DivisionByZero else a `div` b) e) >> nextIns
runCom p (_, Ext s e) = (let eFuncs = extFuncs p; nr = findLabel eFuncs s in snd (eFuncs ^! nr) $ e) >> nextIns
jf f p s = do { a <- readAccum; if f a then jump p s else nextIns; }
accumf f e = do { a <- readAccum; load e; modAccum (f a); }

data Program = Program { commands :: [(EString,BCommand)], extFuncs :: [(EString, EInt -> Command ())], mstate :: MState }
instance Show Program where
  show p = 
    (show $ mstate p) 
    ++ "\nInstruction: " 
    ++ let insNr = curInstr $ mstate p in show insNr 
                                          ++ ": " ++ show (commands p ^! insNr)

stepProgram p = let curCom = runC $ runCom p $ commands p ^! (curInstr $ mstate p) in p { mstate = snd . curCom $ mstate p }

data Flags = Flags Integer
instance Show Flags where
  show f = concatMap (++"\n") $ map fst $ filter snd $ map (\(ES s) -> (s, checkFlag (ES s) f) ) flags
flags = [ES "debug"]
checkFlag :: EString -> Flags -> Bool
checkFlag str (Flags i) = let m = findLabel (zip flags [0..]) str in (i `shiftR` (toEnum (fromEnum m)::Int)) .&. 1 == 1
debugFlag = checkFlag (flags !! 0)

runProgram flags p = do
  print p `when'` (debugFlag flags)
  (do { let p' = stepProgram p in runProgram flags p'; }) `catch` (handleError p)
  where
  handleError :: Program -> RAMException -> IO ()
  handleError p e = print e >> print p 
  when' = flip when
