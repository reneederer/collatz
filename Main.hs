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
import qualified Data.Text as Text
import Debug.Trace

data Indices =
    Indices { row :: Integer
    , column :: Integer
    } deriving (Show)

data Successor = 
    Successor { rowIndex :: Row
    , columnIndex :: Column
    , iterations :: Integer
    }

type Row = Integer
type Column = Integer
type N = Integer

main = do
    conn <- connect 
        defaultConnectInfo {ciUser = "root", ciPassword = "1234", ciDatabase = "collatz"}
    --_ <- execute conn "drop table if exists collatz" []
    _ <- execute conn "drop procedure if exists getCollatzRC" []
    _ <- execute conn "drop procedure if exists getCollatzN" []
    _ <- execute conn "create table if not exists collatz (rowIndex varchar(50), columnIndex varchar(50), successorRowIndex varchar(50), successorColumnIndex varchar(50), iterations varchar(50), formula varchar (500), n varchar(50), primary key (rowIndex, columnIndex), foreign key(successorRowIndex) references collatz(rowIndex))" []

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

    mapM  (\n -> do
        putStrLn $ "n: " ++ show n ++ " "
        --insertCollatz conn n) [100001,100003..299999]
        insertCollatz conn n) [9212113,9212115..9999999]
    close conn




cut2 :: Integer -> Integer
cut2 n = 
    let _ = trace ("cut2 " ++ show n) n
    in
    if n `mod` 2 == 0
    then cut2 $ n `div` 2
    else n




binToDec :: String -> Integer
binToDec s =
    let _ = trace ("binToDec " ++ show s) s
    in
    binToDec1 s 0

binToDec1 :: String -> Integer -> Integer
binToDec1 [] acc = acc
binToDec1 (d:ds) acc =
    let _ = trace ("binToDec1 " ++ show (d:ds) ++ " " ++ show acc) ds
        g = binToDec1 ds ((2*acc) + (read ('0':d:"") :: Integer))
    in g

ld :: Integer -> Integer
ld 1 = 0
ld n = 
    1 + (ld $ n `div` 2)
    

insertCollatz :: MySQLConn -> Integer -> IO Successor
insertCollatz conn n = do
    let _ = trace ("insertCollatz conn " ++ show n) n
    let successorN = cut2 $ n*3+1
    (_, sqlRows) <- query conn "select rowIndex, columnIndex, iterations from collatz where n = ?" [toMySQLText successorN]
    sqlRow <- Streams.read sqlRows
    (successorRowIndex, successorColumnIndex, iterations) <- do
            if n == 1
            then do
                pure (0, 0, 0)
            else
                case sqlRow of
                    Nothing -> do
                        successor <- insertCollatz conn successorN
                        pure (rowIndex successor, columnIndex successor, 1 + iterations successor)
                    Just row -> do
                        pure (fromMySQLText $ row !! 0, fromMySQLText $ row !! 1, (fromMySQLText (row !! 2)) + 1)
    skipToEof sqlRows
    let indices = getIndices n
    let functionName = getCatFunctionName ((n+1) `div` 2) 1 5
    let formulaName = functionName
    let formula = getCatFunctionFromName functionName
    let formulaStr = (snd $ formula !! 0) ++ ", " ++ (snd $ formula !! 1)
    execute
        conn
        "insert ignore into collatz(rowIndex, columnIndex, successorRowIndex, successorColumnIndex, iterations, formula, n) values (?, ?, ?, ?, ?, ?, ?)"
        [ toMySQLText $ row indices
        , toMySQLText $ column indices
        , if successorRowIndex /= 0
          then toMySQLText $ successorRowIndex
          else MySQLNull
        , if successorColumnIndex /= 0
          then toMySQLText $ successorColumnIndex
          else MySQLNull
        , toMySQLText $ iterations
        , MySQLText $ Text.pack formulaStr
        , toMySQLText $ n
        ]
    pure $ Successor { rowIndex = row indices, columnIndex = column indices, iterations = iterations }




getIndices :: N -> Indices
getIndices n = 
    let _ = trace ("getIndices " ++ show n) n
    in
    if (n - 3) `mod` 4 == 0
    then
        Indices { row = (n+1) `div` 2, column = 1 }
    else
        let functionName = getCatFunctionName ((n-1) `div` 2) 1 5
            f = getCatFunctionFromName functionName
        in
        Indices { row = (fst (f !! 0)) ((n-1) `div` 2), column = (fst $ f !! 1) ((n-1) `div` 2) }



--getCatFunctionName 12 1 5
getCatFunctionName :: Row -> Integer -> Integer -> String
getCatFunctionName index modIndex rest =
    let _ = trace ("getCatFunctionName " ++ show index ++ " " ++ show modIndex ++ " " ++ show rest) index
    in
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


xTimes :: Show a => Integer -> [a] -> [a]
xTimes 0 _ = []
xTimes n xs =
    let _ = trace ("xTimes: "++show n ++ " " ++ show xs) xs
    in
    xs ++ (xTimes (n-1) xs)


getCatFunctionFromName :: String -> [((Integer -> Integer), String)]
getCatFunctionFromName name =
    let cat = head name
        _ = trace ("getCatFunctionFromName " ++ name) name
        index = read (tail name) :: Row
        _ = trace ("getCatFunctionFromName " ++ show index) index
    in
    if cat == 'b'
    then
        let s = binToDec $ xTimes ((index+1) `div` 2) "01"
        in
        [ ((\x -> x * (2 ^ index) - (s-1)), "*" ++ show (2 ^ index) ++ "-" ++ show (s-1))
        , ((\_ -> (index `div` 2)+1), "/" ++ show ((index `div` 2)+1))
        ]
    else if cat == 'a'
    then
        let s = binToDec $ "1" ++ (xTimes ((index-1) `div` 2) "01")
        in
        [ ((\x -> (x+((s)+1)) `div` (2 ^ index)), "+" ++ show (s+1) ++ "/" ++ show (2 ^ index))
        , ((\_ -> (index `div` 2) + 1), "*" ++ show ((index `div` 2)+1))
        ]
    else
        []



fromMySQLText ::  MySQLValue -> Integer
fromMySQLText x = 
    let _ = trace ("fromMySQLText " ++ show x) x
    in
    case x of
        MySQLText n -> read (Text.unpack n) :: Integer
        _ -> 0

toMySQLText :: Integer -> MySQLValue
toMySQLText x =
    let _ = trace ("toMySQLText " ++ show x) x
    in
    MySQLText $ Text.pack (show x)



