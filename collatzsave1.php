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
    $statement = $conn-> prepare('create table if not exists collatz (rowIndex int, columnIndex int, successor int, iterations int, formula varchar (500), n int, primary key (rowIndex, columnIndex), foreign key(successor) references collatz(rowIndex)');
    $statement-> execute();
    for($i = 1; $i <= 10; ++$i)
    {
        insertCollatzes(40);
    }   
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
    $statement = $conn->prepare('select successor from collatz where n = ' . $n);
    $result = $statement->execute();
    $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
    $rows = $statement->fetchAll();
    $s = $rows[0]['successor'];
    $acc[] = ($s + (3-$s % 3)) / 3;
    return getCollatz($rows[0]['successor'], $acc);
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

function insertCollatz($n) // returns the successor
{
    global $conn;
    $collatzIndices = getCatIndices($n);
    $successor = $collatzIndices['row']*3 - 1;
    if($successor % 2 == 0)
    {
        --$successor;
    }
    $iterations = 0;
    $functionName = getCatFunctionName(($n-1)/2);
    $formulaName = $functionName[0] . '' . $functionName[1];
    $formula = getCatFunctionFromName($functionName[0], $functionName[1]);
    $formulaStr = $formula[1] . ", " . $formula[3];
    $values = "($n, " . $collatzIndices['row'] . ", " . $collatzIndices['column'] . ", $successor, $iterations, '$formulaStr', '$formulaName')";
    $statement = $conn->prepare('
        insert ignore into collatz(n, rowIndex, columnIndex, successor, iterations, formula, formulaName)
                     values ' . $values);
    $statement->execute();
    return $successor;
}

function insertCollatzes($amount)
{
    global $conn;
    $values = "";
    $low =  maxCollatz() + 2;
    $high = $low + $amount - 1;
    for($i = $low; $i < $high; $i += 2)
    {
        $n = $i;
        $collatzIndices = getCatIndices($n);
        $successor = $collatzIndices['row']*3 - 1;
        if($successor % 2 == 0)
        {
            --$successor;
        }
        $iterations = 0;
        $functionName = getCatFunctionName(($n-1)/2);
        $formulaName = $functionName[0] . '' . $functionName[1];
        $formula = getCatFunctionFromName($functionName[0], $functionName[1]);
        $formulaStr = $formula[1] . ", " . $formula[3];
        $values .= "(" . $collatzIndices['row'] . ", " . $collatzIndices['column'] . ", $successor, $iterations, '$formulaStr', '$formulaName')";
        if($i != $high - 1)
        {
            $values .= ", ";
        }
    }
    $statement = $conn->prepare('
        insert ignore into collatz(rowIndex, columnIndex, successor, iterations, formula, n)
                     values ' . $values);
    $statement->execute();
}


function updateIterations($n, $depth)
{
    global $conn;
    if($n == 1)
    {
        return 0;
    }
    if($depth == 2000)
    {
        return 0;
    }
    $statement = $conn->prepare('select iterations, successor from collatz where n = ' . $n);
    $statement->execute();
    $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
    $rows = $statement->fetchAll();
    $successor = null;
    if(count($rows)==0 && $n != 0)
    {
        $successor = insertCollatz($n);
    }
    else
    {
        if($rows[0]['iterations'] != 0 || $n == 1)
        {
            return $rows[0]['iterations'];
        }
        $successor = $rows[0]['successor'];
    }
    $successorIterations = updateIterations($successor, $depth + 1);
    $statement = $conn->prepare('update collatz set iterations = ' . ($successorIterations + 1) . ' where n = ' . $n);
    $statement->execute();
    return $successorIterations + 1;
}







function getCatIndices($n)
{
    if(($n+1) % 4 == 0)
    {
        return array('row' => ($n+1)/2, 'column' => 1);
    }
    else
    {
        $functionName = getCatFunctionName(($n-1)/2);
        $f = getCatFunctionFromName($functionName[0], $functionName[1]);
        return array('row' => $f[0](($n-1)/2), 'column' => $f[2]);
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

















