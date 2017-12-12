module Main where

-- Import necessary dependecies
import qualified Data.List as List
import qualified Data.Map
import Data.Char
import System.Environment
import System.IO

-- Define a main function to read arguments and do something with them
main :: IO BFInterpreter
main = do
  a <- getArgs
  inf <- openFile (head a) ReadMode
  process_file inf
  
process_file :: Handle -> IO BFInterpreter
process_file h = process_file_r [] h

process_file_r :: [Char] -> Handle -> IO BFInterpreter
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

interpret_program :: [Char] -> IO BFInterpreter
interpret_program c = interpret_program_r (bfi_io_wrap (bfi_init c))

interpret_program_r :: IO BFInterpreter -> IO BFInterpreter
interpret_program_r io_bfi = do
  bfi <- io_bfi
  if bfi_completed bfi then
    return bfi
    else
    if (bfi_next_code bfi) == '.' || (bfi_next_code bfi) == ',' then
      do
        interpret_program_r
        -- Here we increment the code_ptr before we send it to perform
        -- the operation because (1) the code pointer is not needed during
        -- IO operations and (2) we can avoid another unwrap.
          (case bfi_next_code bfi of
             '.' -> bfi_output (bfi_inc_cp bfi)
             ',' -> bfi_input (bfi_inc_cp bfi)
             _ -> io_bfi)
    else
      interpret_program_r (bfi_io_wrap (bfi_inc_cp (
            case bfi_next_code bfi of
              '>' -> (bfi_inc_tp bfi)
              '<' -> (bfi_dec_tp bfi)
              '+' -> (bfi_inc_v bfi)
              '-' -> (bfi_dec_v bfi)
              '[' -> (bfi_jmp_f bfi)
              ']' -> (bfi_jmp_b bfi)
              -- Again, we ignore invalid input (bad practice...)
              _ -> bfi
            )))

bfi_output ::  BFInterpreter -> IO BFInterpreter
bfi_output bfi = do
--  putChar (bfi_get_v_char bfi)
  putStr [(bfi_get_v_char bfi)]
  return bfi

bfi_input :: BFInterpreter -> IO BFInterpreter
bfi_input bfi = do
  inp <- getChar
  return (bfi_set_v bfi (ord inp))

bfi_io_wrap :: BFInterpreter -> IO BFInterpreter
bfi_io_wrap bfi = do
  return bfi

-- Receives a single character from stdin and returns it as an IO monad
--input_ch :: IO Char

-- Prints the passed in Char to stdout
--output_ch :: Char -> IO ()

-- ===========================================
-- === BFInterpreter definitions/inteface  ===
-- ===========================================

-- BFInterpreter encapsulates all data needed to process a .bf
-- file program.
--
-- It is recommended to use all the convenience methods below to modify
-- the values, as they will ensure that edge cases are taken care of.
data BFInterpreter = BF {tape :: [Int],
                         tape_ptr :: Int,
                         code :: [Char],
                         code_ptr :: Int,
                         bracemap :: Data.Map.Map Int Int}

-- Initializes a BFInterpreter, also creating a bracemap in the
-- process. It is recommended to use this method.
bfi_init :: [Char] -> BFInterpreter
bfi_init c = let b = bracemap_for_code c in
  BF [0] 0 c 0 b

-- Convenience methods

bfi_completed :: BFInterpreter -> Bool
bfi_completed bfi = (code_ptr bfi) >= (length (code bfi))

bfi_next_code :: BFInterpreter -> Char
bfi_next_code bfi = ((code bfi) !! (code_ptr bfi))

bfi_inc_cp :: BFInterpreter -> BFInterpreter
bfi_inc_cp bfi = bfi_with_code_ptr bfi ((code_ptr bfi) + 1)

bfi_get_v :: BFInterpreter -> Int
bfi_get_v bfi = (tape bfi) !! (tape_ptr bfi)

bfi_get_v_char :: BFInterpreter -> Char
bfi_get_v_char bfi = Data.Char.chr (bfi_get_v bfi)

-- Sets the input value at the tape_ptr
bfi_set_v :: BFInterpreter -> Int -> BFInterpreter
bfi_set_v bfi inp = bfi_with_tape bfi (
  (take (tape_ptr bfi) (tape bfi) ++ [inp] ++
   (drop ((tape_ptr bfi) + 1) (tape bfi))))

-- Common overwriting methods

bfi_with_tape :: BFInterpreter -> [Int] -> BFInterpreter
bfi_with_tape bfi t = BF t (tape_ptr bfi) (code bfi) (code_ptr bfi)
  (bracemap bfi)

bfi_with_tape_ptr :: BFInterpreter -> Int -> BFInterpreter
bfi_with_tape_ptr bfi t_p = BF (tape bfi) t_p (code bfi) (code_ptr bfi)
  (bracemap bfi)

bfi_with_code_ptr :: BFInterpreter -> Int -> BFInterpreter
bfi_with_code_ptr bfi c_p = BF (tape bfi) (tape_ptr bfi) (code bfi)
  c_p (bracemap bfi)


-- =========================
-- === Operation Methods ===
-- =========================

-- '>'
-- Increments the tape ptr one value.
bfi_inc_tp :: BFInterpreter -> BFInterpreter
bfi_inc_tp bfi = if ((tape_ptr bfi) + 1) >= (length (tape bfi))
                 then
                   bfi_with_tape
                   (bfi_with_tape_ptr bfi ((tape_ptr bfi) + 1))
                   ((tape bfi) ++ [0])
                   else
                   bfi_with_tape_ptr bfi ((tape_ptr bfi) + 1)

-- '<'
-- Decrements the tape ptr one value.
bfi_dec_tp :: BFInterpreter -> BFInterpreter
bfi_dec_tp bfi = if ((tape_ptr bfi) == 0)
                 then
                   bfi_with_tape bfi ([0] ++ (tape bfi))
                   else
                   bfi_with_tape_ptr bfi ((tape_ptr bfi) - 1)

-- '+'
-- Increments the value at tape_ptr one value.
bfi_inc_v :: BFInterpreter -> BFInterpreter
bfi_inc_v bfi = bfi_set_v bfi (
  if (bfi_get_v bfi) < 255 then
    ((bfi_get_v bfi) + 1)
    else
      0
  )
  

-- '-'
-- Decrements the value at tape_ptr one value.
bfi_dec_v :: BFInterpreter -> BFInterpreter
bfi_dec_v bfi = bfi_set_v bfi (
  if (bfi_get_v bfi) > 0 then
    ((bfi_get_v bfi) - 1)
    else
      255
  )

-- '['
-- Moves the code_ptr forward if current value is 0
bfi_jmp_f :: BFInterpreter -> BFInterpreter
bfi_jmp_f bfi
  | (bfi_get_v bfi) == 0 =
    bfi_with_code_ptr bfi ((bracemap bfi)
                            Data.Map.! (code_ptr bfi))
  | otherwise = bfi

-- ']'
-- Moves the code_ptr backwards if current value is not 0
bfi_jmp_b :: BFInterpreter -> BFInterpreter
bfi_jmp_b bfi
  | (bfi_get_v bfi) /= 0 =
    bfi_with_code_ptr bfi ((bracemap bfi)
                            Data.Map.! (code_ptr bfi))
  | otherwise = bfi

-- Returns the 'bracemap' object used by the BFInterpreter given
-- the list of inputs.
bracemap_for_code :: [Char] -> Data.Map.Map Int Int
bracemap_for_code c =
  bracemap_r 0 c [] Data.Map.empty

bracemap_r :: Int -> [Char] -> [Int] -> Data.Map.Map Int Int -> Data.Map.Map Int Int
-- See an opening '['
bracemap_r idx ('[':rest) (stack) m =
  bracemap_r (idx + 1) rest (idx:stack) m
-- See a "proper" ']' (has an opening '[')
bracemap_r idx (']':rest) (prev:stack) m =
  bracemap_r (idx + 1) rest (stack)
  (Data.Map.union m (Data.Map.fromList [(idx, prev), (prev, idx)]))
-- See an "improper" ']' (does not have an opening ']')
-- We choose to ignore it (bad practice!), so this case only
-- exists so we may override it some time in the future
bracemap_r idx (']':rest) [] m =
  bracemap_r (idx + 1) rest [] m
bracemap_r idx (_:rest) stack m =
  bracemap_r (idx + 1) rest stack m
bracemap_r _ _ _ m =
  m
