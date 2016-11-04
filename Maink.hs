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
    _ <- execute conn "drop table if exists collatz" []
    _ <- execute conn "drop procedure if exists getCollatzRC" []
    _ <- execute conn "drop procedure if exists getCollatzN" []
    _ <- execute conn "create table if not exists collatz (rowIndex varchar(50), columnIndex varchar(50), successorRowIndex varchar(50), successorColumnIndex varchar(50), iterations varchar(50), formula varchar (500), n varchar(50), primary key (rowIndex, columnIndex), foreign key(successorRowIndex) references collatz(rowIndex))" []
    mapM  (insertCollatz conn) [1,3..9]
    close conn




cut2 :: Integer -> Integer
cut2 n = 
    if n `mod` 2 == 0
    then cut2 $ n `div` 2
    else n




binToDec :: String -> Integer
binToDec s = binToDec1 s 0
binToDec1 [] acc = acc
binToDec1 (d:ds) acc = binToDec1 ds ((2*acc) + (read (d:"") :: Integer))

ld :: Integer -> Integer
ld 1 = 0
ld n = 
    1 + (ld $ n `div` 2)
    

insertCollatz :: MySQLConn -> Integer -> IO Successor
insertCollatz conn n = do
    let successorN = cut2 $ n*3+1
    (_, sqlRows) <- query conn "select rowIndex, columnIndex, iterations from collatz where n = ?" [toMySQLText successorN]
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
        , toMySQLText $ formulaStr
        , toMySQLText $ n
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
getCatFunctionName :: Row -> Integer -> Integer -> String
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


takeInteger :: Integer -> [a] -> [a]
takeInteger n l = 
    let (_, h) = foldl (\(cn, cl) e -> if cn < n then (cn+1, e:cl) else (cn, cl)) (0, []) l
    in h
    


getCatFunctionFromName :: String -> [((Integer -> Integer), String)]
getCatFunctionFromName name =
    let cat = head name
        index = read (tail name) :: Row
    in
    if cat == 'b'
    then
        let s = binToDec $ concat $ takeInteger ((index+1) `div` 2) (repeat "01")
        in
        [ ((\x -> x * (2 ^ index) - (s-1)), "*" ++ show (2 ^ index) ++ "-" ++ show (s-1))
        , ((\_ -> (index `div` 2)+1), "/" ++ show ((index `div` 2)+1))
        ]
    else if cat == 'a'
    then
        let s = binToDec ("1" ++ (concat $ takeInteger ((index-1) `div` 2) $ repeat "01"))
        in
        [ ((\x -> (x+((s)+1)) `div` (2 ^ index)), "+" ++ show (s+1) ++ "/" ++ show (2 ^ index))
        , ((\_ -> (index `div` 2) + 1), "*" ++ show ((index `div` 2)+1))
        ]
    else
        []



fromMySQLText x = 
    case x of
        MySQLText n -> read (show n) :: Integer
        _ -> 0
toMySQLText x = MySQLText $ Text.pack (show x)



