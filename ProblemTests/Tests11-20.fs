namespace ProblemTests

open Problems_solutions
open Xunit
open FsUnit.Xunit

module Tests11 =
    open NHamcrest

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
    let ``Solution 14 Duplicate Concat`` () =
        checkDuplicatingListCases Solutions11.DuplicateListConcat
    
    [<Fact>]
    let ``Solution 14 Duplicate Recur`` () =
        checkDuplicatingListCases Solutions11.DuplicateListRecur
    
    [<Fact>]
    let ``Solution 14 Duplicate RecurAccum`` () =
        checkDuplicatingListCases Solutions11.DuplicateListRecurAccum
    
    [<Fact>]
    let ``Solution 14 Duplicate Collect`` () =
        checkDuplicatingListCases Solutions11.DuplicateListCollect
    
    [<Fact>]
    let ``Solution 14 Duplicate Fold Back`` () =
        checkDuplicatingListCases Solutions11.DuplicateListFoldBack
    
    [<Fact>]
    let ``Solution 14 Duplicate Fold`` () =
        checkDuplicatingListCases Solutions11.DuplicateListFold
    
    [<Fact>]
    let ``Solution 14 Duplicate CollectRepl`` () =
        checkDuplicatingListCases Solutions11.DuplicateListCollectRepl
    
    [<Fact>]
    let ``Solution 14 Duplicate Yield`` () =
        checkDuplicatingListCases Solutions11.DuplicateListYield
    
    let checkMultiplyListCases fun4test =
        let input = [1; 2; 2; 3; 4; 4; 4]
        let expected = [1;1;1;2;2;2;2;2;2;3;3;3;4;4;4;4;4;4;4;4;4]
        fun4test input 3 |> should equal expected

    [<Fact>]
    let ``Solution 15 Multiply Collect`` () =
        checkMultiplyListCases Solutions11.MultiplyListCollectRepl
    
    [<Fact>]
    let ``Solution 15 Multiply Yield`` () =
        checkMultiplyListCases Solutions11.MultiplyListYield
    
    let checkDropListCases fun4test =
        let input = [1..15]
        let expected = [1;2;3;4;6;7;8;9;11;12;13;14]
        fun4test input 5 |> should equal expected
        let expected = [1;3;5;7;9;11;13;15]
        fun4test input 2 |> should equal expected

    [<Fact>]
    let ``Solution 16 Drop Every nth rec`` () =
        checkDropListCases Solutions11.DropEveryNthElementRec

    [<Fact>]
    let ``Solution 16 Drop Every nth Fold`` () =
        checkDropListCases Solutions11.DropEveryNthElementFold

    [<Fact>]
    let ``Solution 16 Drop Every nth ZipFilter`` () =
        checkDropListCases Solutions11.DropEveryNthElementZipFilter

    [<Fact>]
    let ``Solution 16 Drop Every nth rec acc`` () =
        checkDropListCases Solutions11.DropEveryNthElementRecAccum

    [<Fact>]
    let ``Solution 16 Drop Every nth MapiFilter`` () =
        checkDropListCases Solutions11.DropEveryNthElementMapiFilter

    let checkSplitListCases fun4test =
        let input = [1..15]
        fun4test input 3 |> should equal ([1..3], [4..15])
        
        let (p1,p2) = fun4test input 15 
        p1 |> should equal [1..15]
        p2 |> should be Empty

    [<Fact>]
    let ``Solution 17 Split Rec`` () =
        checkSplitListCases Solutions11.SplitList
    
    [<Fact>]
    let ``Solution 17 Split Rec Down`` () =
        checkSplitListCases Solutions11.SplitList1

    [<Fact>]
    let ``Solution 17 Split Rec alt`` () =
        checkSplitListCases Solutions11.SplitList2

    let checkSliceListCases fun4test =
        let input = [1..15]
        fun4test input 3 6 |> should equal [3..6]
        
        fun4test input 5 17 |> should equal [5..15]
        
        fun4test input 3 3 |> should equal [3]

        fun4test input 3 2 |> should be Empty
        
    [<Fact>]
    let ``Solution 18 Slice`` () =
        checkSliceListCases Solutions11.SliceList

    [<Fact>]
    let ``Solution 18 Slice ZipFilter`` () =
        checkSliceListCases Solutions11.SliceListZipFilter 

    [<Fact>]
    let ``Solution 18 Slice Mapi Filter`` () =
        checkSliceListCases Solutions11.SliceListMapiFilter 

    [<Fact>]
    let ``Solution 18 Slice Zip Lazy`` () =
        checkSliceListCases Solutions11.SliceListZipLazy 
    
    [<Fact>]
    let ``Solution 18 Slice Yield`` () =
        checkSliceListCases Solutions11.SliceListYield

    [<Fact>]
    let ``Solution 18 Slice Yield Alt`` () =
        checkSliceListCases Solutions11.SliceListYieldAlt
        