<?php
    $l_index = 0;
    $l_s = 0;
    $l_f = 0;
    ini_set('display_startup_errors', 1);
    ini_set('display_errors', 1);
    error_reporting(-1);
    $conn = new PDO('mysql:host=localhost;dbname=collatz', 'root', '1234');
    $conn->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
    $statement = $conn->prepare('drop table if exists collatz');
    $statement->execute();
    $statement = $conn-> prepare('create table if not exists collatz (rowIndex int, columnIndex int, successorRowIndex int, iterations int, formula varchar (500), n int, primary key (rowIndex, columnIndex), foreign key(successorRowIndex) references collatz(rowIndex))');
    $statement-> execute();
    //insertCollatzes(1, 51);
    for($i = 1; $i <= 10000; $i += 2)
    {
        insertCollatz($i);
    }
    //updateSuccessors();
    /*while(false)
    {
        $statement = $conn->prepare('select n from collatz where iterations = 0 and n <> 1 limit 1');
        $statement->execute();
        $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
        $rows = $statement->fetchAll();
        if(count($rows) == 0)
        {
            break;
        }
        updateIterations($rows[0]['n'], 0);
    }
//    $maxCollatz = maxCollatz();
    for($i = 1; $i <= 400; $i += 2)
    {
        echo $i . ": ";
        foreach(getCollatz($i, array()) as $current)
        {
            echo $current . " ";
        }
        echo "<br>";
    }
     */
    echo "<br><br><br>Task completed.";




function getCollatz($n, $acc)
{
    global $conn;
    if($n == 1)
    {
        return $acc;
    }
    $statement = $conn->prepare('select successorRowIndex from collatz where n = ' . $n);
    $result = $statement->execute();
    $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
    $rows = $statement->fetchAll();
    $successorRowIndex = $rows[0]['successorRowIndex'];
    $acc[] = ($successorRowIndex + (3-$successorRowIndex % 3)) / 3;
    return getCollatz($successorRowIndex, $acc);
}

function maxCollatz()
{
    global $conn;
    $statement = $conn->prepare('select max(n) as maxN from collatz');
    $statement->execute();
    $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
    $rows = $statement->fetchAll();
    return (is_null($rows[0]['maxN'])) ? -1 : $rows[0]['maxN'];
}



function deleteCollatzes()
{
    global $conn;
    $statement = $conn->prepare('delete from collatz');
    $statement->execute();
}

function cut2($n)
{
    if($n % 2 == 0)
    {
        return cut2($n/2);
    }
    return $n;
}

function insertCollatz($n)
{
    global $conn;
    $successorN = cut2($n*3+1);
    $successorRowIndex = getIndices($successorN)['row'];
    $statement = $conn->prepare("select rowIndex, iterations from collatz where n = " . $successorRowIndex);
    $statement->execute();
    $statement->setFetchMode(PDO::FETCH_ASSOC); 
    $rows = $statement->fetchAll();
    $iterations = 0;
    if($n == 1)
    {
        $successorRowIndex = 'NULL';
        $iterations = 0;
    }
    else
    {
        if(count($rows) == 0)
        {
            $successor = insertCollatz($successorN);
            $successorRowIndex = $successor['rowIndex'];
            $iterations = $successor['iterations'] + 1;
        }
        else
        {
            $successorRowIndex = $rows[0]['rowIndex'];
            $iterations = $rows[0]['iterations'] + 1;
        }
    }
    $indices = getIndices($n);
    $functionName = getCatFunctionName(($n+1)/2);
    $formulaName = $functionName[0] . $functionName[1];
    $formula = getCatFunctionFromName($functionName[0], $functionName[1]);
    $formulaStr = $formula[1] . ", " . $formula[3];
    $rowIndex = $indices['row'];
    $columnIndex = $indices['column'];
    $values = "($rowIndex, $columnIndex, $successorRowIndex, $iterations, '$formulaStr', $n)";
    echo $values;
    $statement = $conn->prepare("insert ignore into collatz(rowIndex, columnIndex, successorRowIndex, iterations, formula, n) values $values");
    $statement->execute();
    return array('rowIndex' => $rowIndex, 'iterations' => $iterations);
}



function updateSuccessors()
{
    global $conn;
    while(true)
    {
        $statement = $conn->prepare("update ignore collatz set successorRowIndex = n*");
        $statement->execute();
//        $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
//        $rows = $statement->fetchAll();
//        if(count($rows) == 0)
//        {
//            return;
//        }
//        $statement = $conn->prepare("select n from collatz where successorRowIndex is NULL and rowIndex <> 1");
//        $statement->execute();
        break;
    }
}

function insertCollatzes($low, $high)
{
    global $conn;
    $values = "";
    for($n = 1; $n <= $high; $n+=2)
    {
        $indices = getIndices($n);
        $functionName = getCatFunctionName(($n+1)/2);
        $formulaName = $functionName[0] . $functionName[1];
        $formula = getCatFunctionFromName($functionName[0], $functionName[1]);
        $formulaStr = $formula[1] . ", " . $formula[3];
        $rowIndex = $indices['row'];
        $columnIndex = $indices['column'];
        $s = 'NULL';
        $iterations = 0;
        $values .= "($rowIndex, $columnIndex, $s, $iterations, '$formulaStr', $n)";
        //echo "row: " . $indices['row'] . ", " . $indices['column'] . ", $formulaName <br>";
        //echo "row: " . $indices['row'] . ", $formulaName <br>";
        if($n != $high)
        {
            $values .= ", ";
        }
    }
    echo $values;
    echo "<br><br>";
    echo $values;
    $statement = $conn->prepare("insert into collatz(rowIndex, columnIndex, successorRowIndex, iterations, formula, n) values $values");
    $statement->execute();
}


function updateIterations($n, $depth)
{
    ///insertCollatz changed!!
    return;
    global $conn;
    if($n == 1)
    {
        return 0;
    }
    if($depth == 2000)
    {
        return 0;
    }
    $statement = $conn->prepare('select iterations, successorRowIndex from collatz where n = ' . $n);
    $statement->execute();
    $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
    $rows = $statement->fetchAll();
    $successorRowIndex = null;
    if(count($rows)==0 && $n != 0)
    {
        $successorRowIndex = insertCollatz($n);
    }
    else
    {
        if($rows[0]['iterations'] != 0 || $n == 1)
        {
            return $rows[0]['iterations'];
        }
        $successorRowIndex = $rows[0]['successorRowIndex'];
    }
    $successorIterations = updateIterations($successorRowIndex, $depth + 1);
    $statement = $conn->prepare('update collatz set iterations = ' . ($successorIterations + 1) . ' where n = ' . $n);
    $statement->execute();
    return $successorIterations + 1;
}




function getIndices($n)
{
    $n -= 2; //TODO bad
    if(($n -1) % 4 == 0)
    {
        return array('row' => ($n+3)/2, 'column' => 1);
    }
    else
    {
        $functionName = getCatFunctionName(($n+1)/2);
        $f = getCatFunctionFromName($functionName[0], $functionName[1]);
        return array('row' => $f[0](($n+1)/2), 'column' => $f[2]);
    }
}





function getCatFunctionName($index)
{
    $rest = 5;
    for($modIndex = 1, $i = 0; $i < 20000000; ++$modIndex, ++$i)
    {
        if($index % (2* (pow(2, $modIndex))) == ($rest % (2* (pow(2, $modIndex)))) ||     ($modIndex % 2 == 0 && (pow(2, $modIndex + 1)+1)/3 === $index))
        {
            return array("b", log((2* (pow(2, $modIndex)))/2, 2));
        }
        if($index % (2* (pow(2, $modIndex))) == ($rest % (2* (pow(2, $modIndex))) - 1) || ($modIndex % 2 == 0 && (pow(2, $modIndex + 1)-2)/3 === $index))
        {
            return array("a", log((2* (pow(2, $modIndex)))/2, 2));
        }
        if($modIndex % 2 == 1)
        {
            $rest = $rest + (pow(2, $modIndex));
        }
        else
        {
            $rest = $rest + 12*(pow(2, ($modIndex-2)));
        }
    }
    return array("not found");
}




function getCatFunctionFromName($name, $index)
{
    global $l_s;
    global $l_index;
    $l = $l_index;
    $l_index = $index;
    if($name == "b")
    {
        $l_s = array('0');
        for($i = 0; $i < floor(($l_index+1) / 2); ++$i)
        {
            $l_s[] = '0';
            $l_s[] = '1';
        }
        return array(
            function($x){ global $l_s; global $l_index; return $x * pow(2, $l_index) - (binToDec($l_s)-1); },
            "*" . pow(2, $l_index) . "-" . (binToDec($l_s)-1),
            floor($l_index/2)+1,
            "*" . (floor($l_index/2)+1),
            );
    }
    else if($name == "a")
    {
        $l_s = array('1');
        for($i = 0; $i < floor(($l_index-1) / 2); ++$i)
        {
            $l_s[] = '0';
            $l_s[] = '1';
        }
        return array(
            function($x){ global $l_s; global $l_index; return ($x+(binToDec($l_s)+1))/pow(2, $l_index); },
            "+" . (binToDec($l_s)+1) . "/" . pow(2, $l_index),
            floor(($l_index/2)+1),
            "/" . (floor($l_index/2)+1),
            );
    }
    else
    {
        return array("hallo welt", "hadsf", "hsdaf", "jdskag");
    }
}



function binToDec($bin)
{
    $re = 0;
    foreach($bin as $c)
    {
        $re *= 2;
        if($c === '1')
        {
            $re += 1;
        }
    }
    return $re;
}

















