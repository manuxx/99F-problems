namespace ProblemTests

open Problems_solutions
open Xunit
open FsUnit.Xunit

module Tests42 =
    [<Fact>]
    let ``Solution 46`` () =
        Solutions42.table Solutions42.and' |> should equal [[false; false; false]; [false; true; false]; [true; false; false]; [true; true; true] ]
        Solutions42.table Solutions42.or' |> should equal  [[false; false; false]; [false; true; true];  [true; false; true];  [true; true; true] ]
        Solutions42.table Solutions42.xor |> should equal  [[false; false; false]; [false; true; true];  [true; false; true];  [true; true; false]]
        Solutions42.table Solutions42.nand |> should equal [[false; false; true] ; [false; true; true];  [true; false; true];  [true; true; false]]
        Solutions42.table Solutions42.nor |> should equal  [[false; false; true] ; [false; true; false]; [true; false; false]; [true; true; false]]
        Solutions42.table Solutions42.equ |> should equal  [[false; false; true] ; [false; true; false]; [true; false; false]; [true; true; true] ]
        Solutions42.table Solutions42.impl|> should equal  [[false; false; true] ; [false; true; true];  [true; false; false]; [true; true; true] ]

    [<Fact>]        
    let ``Solution 47`` () =
        Solutions42.table (&&) |> should equal              [ [false; false; false]; [false; true; false]; [true; false; false]; [true; true; true] ]
        Solutions42.table (||) |> should equal              [ [false; false; false]; [false; true; true];  [true; false; true];  [true; true; true] ]
        Solutions42.table Solutions42.(&|) |> should equal  [ [false; false; false]; [false; true; true];  [true; false; true];  [true; true; false]]
        Solutions42.table Solutions42.(^&&) |> should equal [ [false; false; true] ; [false; true; true];  [true; false; true];  [true; true; false]]
        Solutions42.table Solutions42.(^||) |> should equal [ [false; false; true] ; [false; true; false]; [true; false; false]; [true; true; false]]
        Solutions42.table (=) |> should equal               [ [false; false; true] ; [false; true; false]; [true; false; false]; [true; true; true] ]
        Solutions42.table Solutions42.(|->)|> should equal  [ [false; false; true] ; [false; true; true];  [true; false; false]; [true; true; true] ]

    [<Fact>]       
    let ``Solution 48`` ()  =
        Solutions42.tablen 3 (fun [a;b;c] -> a && (b || c) = a && b || a && c) 
        |> should equal [
                            [true;true;true;true]
                            [false;true;true;false]
                            [true;false;true;true]
                            [false;false;true;false]
                            [true;true;false;true]
                            [false;true;false;false]
                            [true;false;false;false]
                            [false;false;false;false]
                        ]

    [<Fact>]       
    let ``Solution 49`` ()  =
        Solutions42.genGrayCode 3  |> should equal  ["000";"001";"011";"010";"110";"111";"101";"100"]
    
    [<Fact>]       
    let ``Solution 49 rec`` ()  =
        Solutions42.genGrayCodeRec 3  |> should equal  ["000";"001";"011";"010";"110";"111";"101";"100"]

    [<Fact>]       
    let ``Solution 49 rec alt`` ()  =
        Solutions42.genGrayCodeRecAlt 3  
        |> should equal  ["000";"001";"011";"010";"110";"111";"101";"100"]

    [<Fact>]       
    let ``Solution 50`` ()  =
        Solutions42.genHuffman [('a',45);('b',13);('c',12);('d',16);('e',9);('f',5)]  
        |> should equal  [('a', "0"); ('b', "101"); ('c', "100"); ('d', "111"); ('e', "1101"); ('f', "1100")]

    [<Fact>]       
    let ``Solution 50 alt`` ()  =
        Solutions42.huffman [('a',45);('b',13);('c',12);('d',16);('e',9);('f',5)]  
        |> should equal  [('a', "0"); ('b', "101"); ('c', "100"); ('d', "111"); ('e', "1101"); ('f', "1100")]