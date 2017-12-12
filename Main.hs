module Main where

-- Import necessary dependecies
import qualified Data.List as List
import Data.Char
import qualified Data.Map
import System.Environment
import System.IO

-- Define a main function to read arguments and do something with them
main :: IO ()
main = do
  a <- getArgs
  inf <- openFile (head a) ReadMode
  process_file inf
  
process_file :: Handle -> IO ()
process_file h = process_file_r [] h

process_file_r :: [Char] -> Handle -> IO ()
process_file_r chars h = do
  ineof <- hIsEOF h
  if ineof
    then interpret_program chars
    else
    do ch <- (hGetChar h)
       if valid_char ch
         then
         process_file_r (chars ++ [ch]) h
         else (process_file_r chars h)

valid_char :: Char -> Bool
valid_char c = let valids = ['<', '>', '+', '-', '.', ',', '[', ']'] in
  case List.elemIndex c valids of
    Just _ -> True
    Nothing -> False

interpret_program :: [Char] -> IO ()
interpret_program c = let bfi = bfi_with_instrs c in
  interpret_program_r bfi

interpret_program_r :: BFInterpreter -> IO()
interpret_program_r bfi
  | bfi_completed bfi = return ()
  | otherwise = let next_bfi = interpret_program_r (bfi_execute_next bfi) in
      let op = (bfi_next_ip next_bfi) in
        next_bfi

-- =============================
-- === Upper level type defs ===
-- =============================

-- BFInterpreter is the actual machine that handles input, the
-- instruction pointer, and the execution of the code.
data BFInterpreter = BF {tape :: Tape,
                         instr_p :: Int,
                         instrs :: [Char],
                         bracket_map :: Data.Map.Map Int Int}

-- Returns a new, blank BFI
new_bfi :: BFInterpreter
new_bfi = BF blank_tape 0 [] Data.Map.empty

bfi_with_instrs :: [Char] -> BFInterpreter
bfi_with_instrs i = BF blank_tape 0 i Data.Map.empty

-- Returns a BFI with a different tape
--
-- Useful for updating the tape after a calculation
bfi_with_tape :: BFInterpreter -> Tape -> BFInterpreter
bfi_with_tape bfi t = BF t (instr_p bfi) (instrs bfi)
                      (bracket_map bfi)

-- Executes the next step in the BFI's instrs set
bfi_execute_next :: BFInterpreter -> BFInterpreter
bfi_execute_next bfi = let res = bfi_execute_ch (bfi_cur_instr bfi) bfi in
  BF (tape res) (bfi_next_ip bfi) (instrs bfi) (bracket_map bfi)

-- Executes the single char using the BFI
bfi_execute_ch :: Char -> BFInterpreter -> BFInterpreter
bfi_execute_ch c bfi
  | c == '>' || c == '<' || c == '+' || c == '-' =
      bfi_with_tape bfi (tape_execute_ch c $! (tape bfi))
  | c == '.' || c == ',' =
    let t = tape_execute_io c (tape bfi) in
      bfi_with_tape bfi t
  | otherwise = bfi

-- Executes a single char using the tape and it's convenience methods
tape_execute_ch :: Char -> Tape -> Tape
tape_execute_ch c t = t `seq`
                      if c == '>' then increment_dp t
                      else if c == '<' then decrement_dp t
                      else if c == '+' then increase_value t
                      else if c == '-' then decrease_value t
                      else t

tape_execute_io :: Char -> Tape -> IO Tape
tape_execute_io c t = if c == '.' then output_dp t
                      else if c == ',' then input_dp t
                           else return t

bfi_next_ip :: BFInterpreter -> Int
bfi_next_ip bfi = bfi_next_ip_for_ch (bfi_cur_instr bfi) bfi

-- Returns the next instruction pointer given the BFI
bfi_next_ip_for_ch :: Char -> BFInterpreter -> Int
bfi_next_ip_for_ch c bfi
  | c == '[' || c == ']' = let m = (bracket_map bfi) in
      if (cur_value (tape bfi)) == 0 then
        (Data.Map.findWithDefault (instr_p bfi) (instr_p bfi) m) + 1
      else
        (instr_p bfi) + 1
  | otherwise = (instr_p bfi) + 1

-- Returns the current instruction
bfi_cur_instr :: BFInterpreter -> Char
bfi_cur_instr bfi = (instrs bfi) !! (instr_p bfi)

-- Returns True if the interpreter is finished with it's instructions,
-- False otherwise.
bfi_completed :: BFInterpreter -> Bool
bfi_completed bfi = (length (instrs bfi)) <= (instr_p bfi)

-- =======================
-- === Tape Definition ===
-- =======================

-- Tape handles the data and the pointer to the data.
data Tape = Tp {data_p :: Int,
                tape_list :: [Int]}

-- A default constructor for a Tape.
blank_tape :: Tape
blank_tape = Tp 0 [0]

cur_value :: Tape -> Int
cur_value t = (tape_list t) !! (data_p t)

-- A convenience method to get the character that is being pointed
-- to currently.
char_at_head :: Tape -> Char
char_at_head t = Data.Char.chr ((tape_list t) !! (data_p t))

-- ========================
-- === Mapped functions ===
-- ========================

-- '>' operator
-- Increment data point
increment_dp :: Tape -> Tape
increment_dp t
  | length (tape_list t) == data_p t =
    Tp (1 + data_p t) ((tape_list t) ++ [0])
  | otherwise = Tp (1 + data_p t) (tape_list t)

-- '<' operator
-- Decrement data point
decrement_dp :: Tape -> Tape
decrement_dp t
  | 0 == data_p t = Tp (data_p t) ([0] ++ (tape_list t))
  | otherwise = Tp (data_p t - 1) (tape_list t)

-- '+' operator
-- Increase value at dp by one byte
increase_value :: Tape -> Tape
increase_value t = Tp (data_p t)
  (let (s, e) = List.splitAt (data_p t) (tape_list t) in
     s ++ [(head e + 1)] ++ (tail e))

-- '-' operator
-- Decrement value at dp by one byte
decrease_value :: Tape -> Tape
decrease_value t = let p = (data_p t) in
  Tp p ((List.take (p - 1) (tape_list t)) ++
        [(((tape_list t) !! p) - 1)] ++
        (List.drop (p + 1) (tape_list t)))

-- '.' operator
-- Output thebyte at the data pointer
output_dp :: Tape -> IO Tape
output_dp t = do
  putStrLn "Putting a char!"
  putChar (char_at_head t)
  return $! t

-- ',' operator
-- Accept one byte of input, storing its value in the byte
-- at the dp.
input_dp :: Tape -> IO Tape
input_dp t = do c <- getChar
                let v = ord c in
                  let (s, e) = List.splitAt (data_p t) (tape_list t) in
                    let new_t = Tp (data_p t) (s ++ [v] ++ (tail e)) in
                      return $! new_t
