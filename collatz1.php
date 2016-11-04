<?php
    $l_index = 0;
    $l_s = 0;
    $l_f = 0;
    ini_set('display_startup_errors', 1);
    ini_set('display_errors', 1);
    error_reporting(-1);
    $conn = new PDO('mysql:host=localhost;dbname=collatz', 'root', '1234');
    $conn->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
    insertCollatzes(1001);
    for($i = 1; $i < 100; $i += 2)
    {
        $catIndices = getCatIndices($i);
        echo $i . ": " . $catIndices[0] . ", " . $catIndices[1];
        echo "<br>";
    }









/*function load_fc_shapes()
{
    global $conn;
    $statement = $conn->prepare('
        select fc_shape.id as id, x, y, fc_shape_type.name as shapeType, title, text
        from fc_shape
        join book
          on fc_shape.book_id = book.id
        join user
           on book.user_id = :user_id
        join fc_shape_type
          on fc_shape.fc_shape_type_id = fc_shape_type.id
        where
          book_id = (select max(book.id) from book where book.name = :book_name)');
    $statement->bindParam(':user_id', $_SESSION['user_id']);
    $statement->bindParam(':book_name', $_SESSION['book_name']);
    $result = $statement->execute();
    $result = $statement->setFetchMode(PDO::FETCH_ASSOC); 
    $rows = $statement->fetchAll();
    return $rows;
}*/





function insertCollatzes($high)
{
    global $conn;
    $statement = $conn->prepare('delete from collatz');
    $statement->execute();
    $values = "";
    $low =  1;
    $high = 100;
    for($i = $low; $i < $high; $i += 2)
    {
        $n = $i;
        $collatzIndices = getCatIndices($n);
        $successor = $collatzIndices[0]*3 - 1;
        if($successor % 2 == 0)
        {
            --$successor;
        }
        $iterations = 13;
        $values .= "($n, $collatzIndices[0], $collatzIndices[1], $successor, $iterations)";
        if($i != $high - 1)
        {
            $values .= ", ";
        }
    }
    echo $values;
    $statement = $conn->prepare('
        insert into collatz(n, rowIndex, columnIndex, successor, iterations)
                     values ' . $values);
    $statement->execute();
}







function getCatIndices($n)
{
    if(($n+1) % 4 == 0)
    {
        return array(($n+1)/2, 1);
    }
    else
    {
        $functionName = getCatFunctionName(($n-1)/2);
        $f = getCatFunctionFromName($functionName[0], $functionName[1]);
        return array($f[0](($n-1)/2), $f[2]);
    }
}





function getCatFunctionName($index)
{
    $rest = 5;
    for($modIndex = 1, $i = 0; $i < 20000000; ++$modIndex, ++$i)
    {
        if($index % (2* (pow(2, $modIndex))) == ($rest % (2* (pow(2, $modIndex)))) ||     ($modIndex % 2 == 0 && (pow(2, $modIndex + 1)+1)/3 === $index))
        {
            return array("a", log((2* (pow(2, $modIndex)))/2, 2));
        }
        if($index % (2* (pow(2, $modIndex))) == ($rest % (2* (pow(2, $modIndex))) - 1) || ($modIndex % 2 == 0 && (pow(2, $modIndex + 1)-2)/3 === $index))
        {
            return array("b", log((2* (pow(2, $modIndex)))/2, 2));
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
    if($name == "a")
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
    else if($name == "b")
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





/*
function getCollatzIndices($n)
{
    $catCount = ($n+1)/2;
    $row = 1;
    $column = 1;
    for($currentCatIndex = 1; $currentCatIndex < $catCount; ++$currentCatIndex)
    {
        if($currentCatIndex % 4 == (5 % 4)) //a1
        {
            $row = $row*2-0;
            $column = $column/1;
        }
        else if($currentCatIndex % 4 == (4 % 4)) //b1
        {
            $row = ($row+2)/2;
            $column = $column*1;
        }
        else if($currentCatIndex % 8 == 7 || $currentCatIndex == 3) //a2
        {
            $row = ($row*4)-0;
            $column = $column/2;
        }
        else if($currentCatIndex % 8 == 6 || $currentCatIndex == 3) //b2
        {
            $row = ($row+2)/4;
            $column = $column*2;
        }
        else if($currentCatIndex % 16 == (19 % 16)) //a3
        {
            $row = ($row*8)-4;
            $column = $column/2;
        }
        else if($currentCatIndex % 16 == (18 % 16)) //b3
        {
            $row = ($row+6)/8;
            $column = $column*2;
        }
        else if($currentCatIndex % 32 == 27 || $currentCatIndex == 11) //a4
        {
            $row = ($row*16)-4;
            $column = $column/3;
        }
        else if($currentCatIndex % 32 == 26 || $currentCatIndex == 10) //b4
        {
            $row = ($row+6)/16;
            $column = $column*3;
        }
        else if($currentCatIndex % 64 == (75 % 64)) //a5
        {
            $row = ($row*32)-20;
            $column = $column/3;
        }
        else if($currentCatIndex % 64 == (74 % 64)) //b5
        {
            $row = ($row+22)/32;
            $column = $column*3;
        }
        else if($currentCatIndex % 128 == 107 || $currentCatIndex == 43) //a6
        {
            $row = ($row*64)-20;
            $column = $column/4;
        }
        else if($currentCatIndex % 128 == 106 || $currentCatIndex == 42) //b6
        {
            $row = ($row+22)/64;
            $column = $column*4;
        }
        else if($currentCatIndex % 256 == (299 % 256)) //a7
        {
            $row = ($row*128)-84;
            $column = $column/4;
        }
        else if($currentCatIndex % 256 == (298 % 256)) //b7
        {
            $row = ($row+86)/128;
            $column = $column*4;
        }
    }
    return array($row, $column);
}

*/

















