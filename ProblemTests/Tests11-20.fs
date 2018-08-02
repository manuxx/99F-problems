namespace ProblemTests

open Problems_solutions
open Xunit
open FsUnit.Xunit

module Tests11 =

    let checkRLEEncodingCases fun4Test =
        let input = [1; 2; 2; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [Solutions11.Single 1; Solutions11.Multiple (3, 2); Solutions11.Multiple (2, 3); Solutions11.Single 2; Solutions11.Single 3; Solutions11.Multiple (3, 2); Solutions11.Multiple (2, 5)]
        fun4Test input |> should equal expected

    [<Fact>]
    let ``Solution 11 RLE modified encoding`` () =
        checkRLEEncodingCases Solutions11.RunLenghtEncodingMod 

    let checkRLEDecodingCases fun4Test =
        let input = [Solutions11.Single 1; Solutions11.Multiple (3, 2); Solutions11.Multiple (2, 3); Solutions11.Single 2; Solutions11.Single 3; Solutions11.Multiple (3, 2); Solutions11.Multiple (2, 5)]
        let expected = [1; 2; 2; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        fun4Test input |> should equal expected

    [<Fact>]
    let ``Solution 12 RLE decoding`` () =
        checkRLEDecodingCases Solutions11.RunLenghtDecode

    [<Fact>]
    let ``Solution 12 RLE decoding alt`` () =
        checkRLEDecodingCases Solutions11.RunLenghtDecodeAlt 

    [<Fact>]
    let ``Solution 13 Direct RLE encoding`` () =
        checkRLEEncodingCases Solutions11.RunLenghtEncodingDirect 
    
    [<Fact>]
    let ``Solution 13 Direct RLE encoding Alt`` () =
        checkRLEEncodingCases Solutions11.RunLenghtEncodingDirectAlt 
    
    [<Fact>]
    let ``Solution 13 Direct RLE encoding FoldBack`` () =
        checkRLEEncodingCases Solutions11.RunLenghtEncodingDirectFoldBack 
    
    [<Fact>]
    let ``Solution 13 Direct RLE encoding FoldBackAltFun`` () =
        checkRLEEncodingCases Solutions11.RunLenghtEncodingDirectFoldBackAltFun 

    let checkDuplicatingListCases fun4test =
        let input = [1; 2; 2; 3; 4; 4; 4]
        let expected = [1;1;2;2;2;2;3;3;4;4;4;4;4;4]
        fun4test input |> should equal expected

    [<Fact>]
    let ``Solution 14 Duplicate`` () =
        checkDuplicatingListCases Solutions11.DuplicateList
    
    [<Fact>]
    let ``Solution 14 Duplicate Fold`` () =
        checkDuplicatingListCases Solutions11.DuplicateListFold
    
    [<Fact>]
    let ``Solution 14 Duplicate Fold Back`` () =
        checkDuplicatingListCases Solutions11.DuplicateListFoldBack
    