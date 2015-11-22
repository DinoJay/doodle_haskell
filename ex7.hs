-- -- file: ch16/csv1.hs
-- import Text.ParserCombinators.Parsec
--
-- {- A CSV file contains 0 or more lines, each of which is terminated
--    by the end-of-line character (eol). -}
-- csvFile :: GenParser Char st [[String]]
-- csvFile =
--     do result <- many line
--        eof
--        return result
--
-- -- Each line contains 1 or more cells, separated by a comma
-- line :: GenParser Char st [String]
-- line =
--     do result <- cells
--        eol                       -- end of line
--        return result
--
-- -- Build up a list of cells.  Try to parse the first cell, then figure out
-- -- what ends the cell.
-- cells :: GenParser Char st [String]
-- cells =
--     do first <- cellContent
--        next <- remainingCells
--        return (first : next)
--
-- -- The cell either ends with a comma, indicating that 1 or more cells follow,
-- -- or it doesn't, indicating that we're at the end of the cells for this line
-- remainingCells :: GenParser Char st [String]
-- remainingCells =
--     (char ',' >> cells)            -- Found comma?  More cells coming
--     <|> (return [])                -- No comma?  Return [], no more cells
--
-- -- Each cell contains 0 or more characters, which must not be a comma or
-- -- EOL
-- cellContent :: GenParser Char st String
-- cellContent =
--     many (noneOf ",\n")
--
--
-- -- The end of line character is \n
-- eol :: GenParser Char st Char
-- eol = char '\n'
--
-- parseCSV :: String -> Either ParseError [[String]]
-- parseCSV input = parse csvFile "(unknown)" input

-- This looks for letters, then spaces, then digits.
-- we then return letters and digits in a tuple.
--
-- I import qualified so that it's clear which
-- functions are from the parsec library:
import qualified Text.Parsec as Parsec

-- I am the error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)

-- alias Parsec.parse for more concise usage in my examples:
parse rule text = Parsec.parse rule "(source)" text

myParser :: Parsec.Parsec String () String
myParser = do
    l1 <- Parsec.many1 Parsec.letter
    Parsec.char ','
    l2 <- Parsec.many1 Parsec.letter
    Parsec.char ','
    return l1 ++ l2

-- mySeparator :: Parsec.Parsec String () ()
mySeparator = do
   -- Parsec.spaces
   Parsec.char ','
   -- Parsec.spaces

--I want to return a list of pairs, this time.
-- cells :: Parsec.Parsec String () [(String, String)]
cells = Parsec.sepBy myParser mySeparator

main = do
    -- f <- readFile("test.txt")
    -- let l = lines f
    -- let p = map (parse cells) l
    -- print p
    print $ parse myParser "saadssaddsa,"
-- import Data.Text as T
--
-- import Text.Parsec
-- import Text.Parsec.Prim
-- import Text.Parsec.Text
--
-- input = T.pack "xxxxxxxxxxxxxxyyyyxxxxxxxxxp"
--
-- parser = do
--   x1 <- many1 (char 'x')
--   y <- many1 (char 'y')
--   x2 <- many1 (char 'x')
--   return (T.pack x1, T.pack y, T.pack x2)
--
-- test = runParser parser () "test" input

--
-- {-# LANGUAGE OverloadedStrings #-}
-- import Data.Attoparsec.Char8
-- import Data.Word
--
-- -- | Type for IP's.
-- data IP = IP Word8 Word8 Word8 Word8 deriving Show
--
-- parseIP :: Parser IP
-- parseIP = do
--   d1 <- decimal
--   char '.'
--   d2 <- decimal
--   char '.'
--   d3 <- decimal
--   char '.'
--   d4 <- decimal
--   return $ IP d1 d2 d3 d4
--
-- main :: IO ()
-- main = print $ parseOnly parseIP "131.45.68.123"
--
