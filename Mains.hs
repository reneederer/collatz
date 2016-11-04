{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.List
import Data.Maybe
import System.IO
import Database.MySQL.Base
import qualified  System.IO.Streams as Streams
import Control.Monad.Loops
import Control.Monad
import Data.Char
import Data.Int
import qualified Data.ByteString.Lazy as BStr
import qualified Data.Text as Text
import Debug.Trace

data Indices =
    Indices { row :: Int
    , column :: Int
    } deriving (Show)

data Successor = 
    Successor { rowIndex :: Row
    , columnIndex :: Column
    , iterations :: Int
    }

type Row = Int
type Column = Int
type N = Int

main = do
    conn <- connect 
        defaultConnectInfo {ciUser = "root", ciPassword = "1234", ciDatabase = "collatz"}
    _ <- execute conn "drop table if exists collatz" []
    _ <- execute conn "drop procedure if exists getCollatzRC" []
    _ <- execute conn "drop procedure if exists getCollatzN" []
    _ <- execute conn
        "CREATE PROCEDURE getCollatzRC(currentRow int, currentColumn int) \n\
        \ BEGIN \n\
        \ DECLARE v_rowIndex int; \n\
        \ DECLARE v_successorRowIndex int; \n\
        \ DECLARE v_columnIndex int; \n\
        \ DECLARE v_successorColumnIndex int; \n\
        \ declare i int; \n\
        \ set i =0; \n\
        \ SET v_successorRowIndex = 1; \n\
        \ SET v_successorColumnIndex = 1; \n\
        \ SET v_rowIndex = currentRow; \n\
        \ SET v_columnIndex = currentColumn; \n\
        \ select successorRowIndex into v_successorRowIndex from collatz where rowIndex = v_rowIndex and columnIndex = v_columnIndex; \n\
        \ select successorColumnIndex into v_successorColumnIndex from collatz where rowIndex = v_rowIndex and columnIndex = v_columnIndex; \n\
        \ create TEMPORARY  table IF NOT EXISTS tempCollatzTable as (select * from collatz where 1=0); \n\
        \ truncate table tempCollatzTable; \n\
        \ WHILE v_rowIndex IS NOT NULL and i < 30000 DO \n\
          \ INSERT INTO tempCollatzTable SELECT * FROM collatz WHERE rowIndex = v_rowIndex AND columnIndex = v_columnIndex; \n\
          \ SET v_rowIndex = v_successorRowIndex; \n\
          \ SET v_columnIndex = v_successorColumnIndex; \n\
          \ set i = i + 1; \n\
          \ select successorRowIndex into v_successorRowIndex from collatz where rowIndex = v_rowIndex and columnIndex = v_columnIndex; \n\
          \ select successorColumnIndex into v_successorColumnIndex from collatz where rowIndex = v_rowIndex and columnIndex = v_columnIndex; \n\
        \ END WHILE; \n\
        \ SELECT * FROM tempCollatzTable; \n\
        \ END" []
    _ <- execute conn
        "CREATE PROCEDURE getCollatzN(v_n int) \n\
        \ BEGIN \n\
        \ DECLARE v_rowIndex int; \n\
        \ DECLARE v_columnIndex int; \n\
        \ set v_rowIndex = 1; \n\
        \ set v_columnIndex = 1; \n\
        \ select rowIndex into v_rowIndex from collatz where n = v_n; \n\
        \ select columnIndex into v_columnIndex from collatz where n = v_n; \n\
        \ call getCollatzRC(v_rowIndex, v_columnIndex); \n\
        \ end" []
    _ <- execute conn "create table if not exists collatz (rowIndex int, columnIndex int, successorRowIndex int, successorColumnIndex int, iterations int, formula varchar (500), n int, primary key (rowIndex, columnIndex), foreign key(successorRowIndex) references collatz(rowIndex))" []
    mapM  (insertCollatz conn) [1,3..199999]
    close conn




cut2 n = 
    if n `mod` 2 == 0
    then cut2 $ n `div` 2
    else n




binToDec :: String -> Int
binToDec s = binToDec1 s 0
binToDec1 [] acc = acc
binToDec1 (d:ds) acc = binToDec1 ds ((2*acc) + (read (d:"") :: Int))

ld :: Int -> Int
ld 1 = 0
ld n = 
    1 + (ld $ n `div` 2)
    

insertCollatz :: MySQLConn -> Int -> IO Successor
insertCollatz conn n = do
    let successorN = cut2 $ n*3+1
    (_, sqlRows) <- query conn "select rowIndex, columnIndex, iterations from collatz where n = ?" [toMySQLInt32 successorN]
    sqlRow <- Streams.read sqlRows
    (successorRowIndex, successorColumnIndex, iterations) <-
            if n == 1
            then pure (0, 0, 0)
            else
                case sqlRow of
                    Nothing -> do
                        successor <- insertCollatz conn successorN
                        pure (rowIndex successor, columnIndex successor, 1 + iterations successor)
                    Just row -> do
                        pure (fromMySQLInt32 $ row !! 0, fromMySQLInt32 $ row !! 1, (fromMySQLInt32 (row !! 2)) + 1)
    skipToEof sqlRows
    let indices = getIndices n
    let functionName = getCatFunctionName ((n+1) `div` 2) 1 5
    let formulaName = functionName
    let formula = getCatFunctionFromName functionName
    let formulaStr = (snd $ formula !! 0) ++ ", " ++ (snd $ formula !! 1)
    execute
        conn
        "insert ignore into collatz(rowIndex, columnIndex, successorRowIndex, successorColumnIndex, iterations, formula, n) values (?, ?, ?, ?, ?, ?, ?)"
        [ toMySQLInt32 $ row indices
        , toMySQLInt32 $ column indices
        , if successorRowIndex /= 0
          then toMySQLInt32 $ successorRowIndex
          else MySQLNull
        , if successorColumnIndex /= 0
          then toMySQLInt32 $ successorColumnIndex
          else MySQLNull
        , toMySQLInt32 $ iterations
        , toMySQLText $ formulaStr
        , toMySQLInt32 $ n
        ]
    pure $ Successor { rowIndex = row indices, columnIndex = column indices, iterations = iterations }




getIndices :: N -> Indices
getIndices n = 
    if (n - 3) `mod` 4 == 0
    then
        Indices { row = (n+1) `div` 2, column = 1 }
    else
        let functionName = getCatFunctionName ((n-1) `div` 2) 1 5
            f = getCatFunctionFromName functionName
        in
        Indices { row = (fst (f !! 0)) ((n-1) `div` 2), column = (fst $ f !! 1) ((n-1) `div` 2) }



--getCatFunctionName 12 1 5
--
getCatFunctionName :: Row -> Int -> Int -> String
getCatFunctionName index modIndex rest =
    if  index `mod` (2* (2 ^ modIndex)) == (rest `mod` (2* (2^modIndex))) ||     (modIndex `mod` 2 == 0 && (2^(modIndex + 1)+1) `div` 3 == index)
    then
        "b" ++ (show $ ld((2* (2^modIndex)) `div` 2))
    else if index `mod` (2* (2^modIndex)) == (rest `mod` (2* (2^modIndex)) - 1) || (modIndex `mod` 2 == 0 && (2^(modIndex + 1)-2) `div` 3 == index)
    then "a" ++ (show $ ld ((2* (2^modIndex)) `div` 2))
    else if modIndex `mod` 2 == 1
    then
        getCatFunctionName index (modIndex + 1) (rest + (2^modIndex))
    else
        getCatFunctionName index (modIndex + 1) (rest + 12*(2^(modIndex-2)))



getCatFunctionFromName :: String -> [((Int -> Int), String)]
getCatFunctionFromName name =
    let cat = head name
        index = read (tail name) :: Row
    in
    if cat == 'b'
    then
        let s = binToDec $ concat $ take ((index+1) `div` 2) (repeat "01")
        in
        [ ((\x -> x * (2 ^ index) - (s-1)), "*" ++ show (2 ^ index) ++ "-" ++ show (s-1))
        , ((\_ -> (index `div` 2)+1), "/" ++ show ((index `div` 2)+1))
        ]
    else if cat == 'a'
    then
        let s = binToDec ("1" ++ (concat $ take ((index-1) `div` 2) $ repeat "01"))
        in
        [ ((\x -> (x+((s)+1)) `div` (2 ^ index)), "+" ++ show (s+1) ++ "/" ++ show (2 ^ index))
        , ((\_ -> (index `div` 2) + 1), "*" ++ show ((index `div` 2)+1))
        ]
    else
        []



fromMySQLInt32 x = 
    case x of
        MySQLInt32 n -> read (show n) :: Int
        _ -> 0
toMySQLInt32 x = MySQLInt32 $ (read (show x) :: Int32)
toMySQLText x = MySQLText $ Text.pack x
