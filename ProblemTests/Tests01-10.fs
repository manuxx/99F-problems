namespace ProblemTests

open Problems_solutions
open Xunit
open FsUnit.Xunit

module Tests =
    [<Fact>]
    let ``Solution 1 rec`` () =
        let input = ["abc"; "def"; "xxx"; "yyy"]
        let result = Solutions.LastListElement1 input
        result |> should equal (Some "yyy")

    let checkLastListElementCases fun4test =
        let input = ["abc"; "def"; "xxx"; "yyy"]
        fun4test input |> should equal "yyy"

    [<Fact>]
    let ``Solution 1 with reverse`` () =
        checkLastListElementCases Solutions.LastListElement2 
    
    [<Fact>]
    let ``Solution 1 with reduce`` () =
        checkLastListElementCases Solutions.LastListElement3 

    [<Fact>]
    let ``Solution 2 rec`` () =
        let list = ["abc"; "def"; "xxx"; "yyy"]
        let last = Solutions.ButLastListElement1 list
        last |> should equal (Some "xxx")

    let checkButLastListElementCases fun4test =
        let input = ["abc"; "def"; "xxx"; "yyy"]
        fun4test input |> should equal "xxx"

    [<Fact>]
    let ``Solution 2 reverseA`` () =
        checkButLastListElementCases Solutions.ButLastListElement2a 

    [<Fact>]
    let ``Solution 2 reverseB`` () =
        checkButLastListElementCases Solutions.ButLastListElement2b 

    [<Fact>]
    let ``Solution 2 flip`` () =
        checkButLastListElementCases Solutions.ButLastListElement3 
    
    [<Fact>]
    let ``Solution 2 item`` () =
        checkButLastListElementCases Solutions.ButLastListElement4 

    [<Fact>]
    let ``Solution 3 rec`` () =
        let list = ["abc"; "def"; "xxx"; "yyy"; "zzz"]
        let last = Solutions.KthListElement1 list 3
        last |> should equal (Some "xxx")

    let checkKthListElementCases fun4test =
        let list = ["abc"; "def"; "xxx"; "yyy"; "zzz"]
        let last = fun4test list 3
        last |> should equal "xxx"

    [<Fact>]
    let ``Solution 3 skip`` () =
        checkKthListElementCases Solutions.KthListElement2

    [<Fact>]
    let ``Solution 3 item`` () =
        checkKthListElementCases Solutions.KthListElement3 

    let checkListLengthCases fun4test =
        fun4test [] |> should equal 0
        let list = ["abc"; "def"; "xxx"; "yyy"; "zzz"]
        fun4test list |> should equal list.Length

    [<Fact>]
    let ``Solution 4 rec`` () =
        checkListLengthCases Solutions.ListLenght1

    [<Fact>]
    let ``Solution 4 rec accum`` () =
        checkListLengthCases Solutions.ListLenght2 

    [<Fact>]
    let ``Solution 4 len`` () =
        checkListLengthCases  Solutions.ListLenght3 

    [<Fact>]
    let ``Solution 4 sumby`` () =
        checkListLengthCases Solutions.ListLenght4 
    
    [<Fact>]
    let ``Solution 4 fold`` () =
        checkListLengthCases  Solutions.ListLenght4 

    let checkListReverseCases fun4test =
        fun4test [] |> should be Empty
        fun4test ["abc"] |> should equal ["abc"]
        
        let list = ["abc"; "def"; "xxx"]
        fun4test list |> should equal ["xxx"; "def"; "abc"]

    [<Fact>]
    let ``Solution 5 rev`` () =
        checkListReverseCases Solutions.ListRev1 

    [<Fact>]
    let ``Solution 5 rec acc`` () =
        checkListReverseCases Solutions.ListRev2 
    
    [<Fact>]
    let ``Solution 5 rec acc function`` () =
        checkListReverseCases Solutions.ListRev3 

    [<Fact>]
    let ``Solution 5 rec alternative function syntax`` () =
        checkListReverseCases Solutions.ListRev3a 

    [<Fact>]
    let ``Solution 5 fold`` () =
        checkListReverseCases Solutions.ListRev4 
    
    [<Fact>]
    let ``Solution 6`` () =
        let list = ["a"; "b"; "a"]
        let isPalindrome = Solutions.IsListPalindrome list
        isPalindrome |> should be True

    let checkFlattenListCases fun4test =
        let input = Solutions.List []
        let result = fun4test input
        result |> should be Empty

        let input = Solutions.List [Solutions.List[]; Solutions.List []]
        let result = fun4test input
        result |> should be Empty

        let input = Solutions.List [Solutions.List[Solutions.List []] ]
        let result = fun4test input
        result |> should be Empty

        let input = Solutions.List [ Solutions.Elem 1; Solutions.List [Solutions.Elem 2; Solutions.List [Solutions.Elem 3; Solutions.Elem 4]; Solutions.Elem 5]]
        let expected = [1;2;3;4;5]
        let result = fun4test input
        result |> should equal expected

    [<Fact>]
    let ``Solution 7 rec`` () =
        checkFlattenListCases Solutions.FlattenList1 

    [<Fact>]
    let ``Solution 7 collect`` () =
        checkFlattenListCases Solutions.FlattenList2 

    [<Fact>]
    let ``Solution 7 collect with twist`` () =
        checkFlattenListCases Solutions.FlattenList2b 

    [<Fact>]
    let ``Solution 7 fold`` () =
        checkFlattenListCases Solutions.FlattenList3 
    
    [<Fact>]
    let ``Solution 7 fold with twist`` () =
        checkFlattenListCases Solutions.FlattenList3b 

    let checkRemoveConsecutivesCases fun4test =
        let input = [1; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [1;2;3;2;3;2;5]
        let result = fun4test input
        result |> should equal expected

    [<Fact>]
    let ``Solution 8 recursive`` () =
        checkRemoveConsecutivesCases Solutions.RemoveConsecutiveDuplicates1 
    
    [<Fact>]
    let ``Solution 8 fold`` () =
        checkRemoveConsecutivesCases Solutions.RemoveConsecutiveDuplicates2 

    [<Fact>]
    let ``Solution 8 fold improved`` () =
        checkRemoveConsecutivesCases Solutions.RemoveConsecutiveDuplicates2b 
    
    [<Fact>]
    let ``Solution 8 foldBack`` () =
        checkRemoveConsecutivesCases Solutions.RemoveConsecutiveDuplicates3 
    
    let checkPackConsecutivesCases fun4test =
        let input = [1; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [[1];[2];[3;3];[2];[3];[2;2;2];[5;5]]
        let result = fun4test input
        result |> should equal expected
    
    [<Fact>]
    let ``Solution 9`` () =
        checkPackConsecutivesCases Solutions.PackConsecutiveDuplicates1 
    
    [<Fact>]
    let ``Solution 9 alt`` () =
        checkPackConsecutivesCases Solutions.PackConsecutiveDuplicates1a 
    
    [<Fact>]
    let ``Solution 9 alt''`` () =
        checkPackConsecutivesCases Solutions.PackConsecutiveDuplicates1a 

    let checkRLEEncodingCases fun4test =
        let input = [1; 2; 2; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [(1, 1);(3, 2);(2, 3); (1, 2); (1, 3); (3, 2); (2, 5)]
        let result = fun4test input
        result |> should equal expected
    
    [<Fact>]
    let ``Solution 10 map`` () =
        checkRLEEncodingCases Solutions.RunLenghtEncoding1 

    [<Fact>]
    let ``Solution 10 alt`` () =
        checkRLEEncodingCases Solutions.RunLenghtEncoding2

    [<Fact>]
    let ``Solution 10 foldback`` () =
        checkRLEEncodingCases Solutions.RunLenghtEncoding3smpl 
    
    [<Fact>]
    let ``Solution 10 foldback fancy`` () =
        checkRLEEncodingCases Solutions.RunLenghtEncoding3fancy 