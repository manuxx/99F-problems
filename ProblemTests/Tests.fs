namespace ProblemTests

open Problems_solutions
open Xunit
open FsUnit.Xunit

module Tests =

    [<Fact>]
    let ``Solution 1 rec`` () =
        let list = ["abc"; "def"; "xxx"; "yyy"]
        let last = Solutions.LastListElement1 list
        last |> should equal (Some "yyy")

    [<Fact>]
    let ``Solution 1 with reverse`` () =
        let list = ["abc"; "def"; "xxx"; "yyy"]
        let last = Solutions.LastListElement2 list
        last |> should equal "yyy"
    
    [<Fact>]
    let ``Solution 1 with reduce`` () =
        let list = ["abc"; "def"; "xxx"; "yyy"]
        let last = Solutions.LastListElement3 list
        last |> should equal "yyy"

    [<Fact>]
    let ``Solution 2 rec`` () =
        let list = ["abc"; "def"; "xxx"; "yyy"]
        let last = Solutions.ButLastListElement1 list
        last |> should equal (Some "xxx")

    [<Fact>]
    let ``Solution 2 reverseA`` () =
        let list = ["abc"; "def"; "xxx"; "yyy"]
        let last = Solutions.ButLastListElement2a list
        last |> should equal  "xxx"

    [<Fact>]
    let ``Solution 2 reverseB`` () =
        let list = ["abc"; "def"; "xxx"; "yyy"]
        let last = Solutions.ButLastListElement2b list
        last |> should equal "xxx"

    [<Fact>]
    let ``Solution 2 flip`` () =
        let list = ["abc"; "def"; "xxx"; "yyy"]
        let last = Solutions.ButLastListElement3 list
        last |> should equal "xxx"
    
    [<Fact>]
    let ``Solution 2 item`` () =
        let list = ["abc"; "def"; "xxx"; "yyy"]
        let last = Solutions.ButLastListElement4 list
        last |> should equal "xxx"

    [<Fact>]
    let ``Solution 3 rec`` () =
        let list = ["abc"; "def"; "xxx"; "yyy"; "zzz"]
        let last = Solutions.KthListElement1 list 3
        last |> should equal (Some "xxx")

    [<Fact>]
    let ``Solution 3 skip`` () =
        let list = ["abc"; "def"; "xxx"; "yyy"; "zzz"]
        let last = Solutions.KthListElement2 list 3
        last |> should equal "xxx"

    [<Fact>]
    let ``Solution 3 item`` () =
        let list = ["abc"; "def"; "xxx"; "yyy"; "zzz"]
        let last = Solutions.KthListElement3 list 3
        last |> should equal "xxx"

    [<Fact>]
    let ``Solution 4 rec`` () =
        let list = ["abc"; "def"; "xxx"; "yyy"; "zzz"]
        let len = Solutions.ListLenght1 list
        len |> should equal list.Length

    [<Fact>]
    let ``Solution 4 rec accum`` () =
        let list = ["abc"; "def"; "xxx"; "yyy"; "zzz"]
        let len = Solutions.ListLenght2 list
        len |> should equal list.Length

    [<Fact>]
    let ``Solution 4 len`` () =
        let list = ["abc"; "def"; "xxx"; "yyy"; "zzz"]
        let len = Solutions.ListLenght3 list
        len |> should equal list.Length

    [<Fact>]
    let ``Solution 4 sumby`` () =
        let list = ["abc"; "def"; "xxx"; "yyy"; "zzz"]
        let len = Solutions.ListLenght4 list
        len |> should equal list.Length
    
    [<Fact>]
    let ``Solution 4 fold`` () =
        let list = ["abc"; "def"; "xxx"; "yyy"; "zzz"]
        let len = Solutions.ListLenght4 list
        len |> should equal list.Length

    [<Fact>]
    let ``Solution 5 rev`` () =
        let list = ["abc"; "def"; "xxx"]
        let len = Solutions.ListRev1 list
        len |> should equal ["xxx"; "def"; "abc"]

    [<Fact>]
    let ``Solution 5 rec acc`` () =
        let list = ["abc"; "def"; "xxx"]
        let len = Solutions.ListRev2 list
        len |> should equal ["xxx"; "def"; "abc"]
    
    [<Fact>]
    let ``Solution 5 rec acc function`` () =
        let list = ["abc"; "def"; "xxx"]
        let len = Solutions.ListRev3 list
        len |> should equal ["xxx"; "def"; "abc"]

    [<Fact>]
    let ``Solution 5 rec alternative function syntax`` () =
        let list = ["abc"; "def"; "xxx"]
        let len = Solutions.ListRev3a list
        len |> should equal ["xxx"; "def"; "abc"]

    [<Fact>]
    let ``Solution 5 fold`` () =
        let list = ["abc"; "def"; "xxx"]
        let len = Solutions.ListRev4 list
        len |> should equal ["xxx"; "def"; "abc"]
    
    [<Fact>]
    let ``Solution 6`` () =
        let list = ["a"; "b"; "a"]
        let isPalindrome = Solutions.IsListPalindrome list
        isPalindrome |> should be True

    [<Fact>]
    let ``Solution 7 rec`` () =
        let input = Solutions.List [ Solutions.Elem 1; Solutions.List [Solutions.Elem 2; Solutions.List [Solutions.Elem 3; Solutions.Elem 4]; Solutions.Elem 5]]
        let expected = [1;2;3;4;5]
        let result = Solutions.FlattenList1 input
        result |> should equal expected

    [<Fact>]
    let ``Solution 7 collect`` () =
        let input = Solutions.List [ Solutions.Elem 1; Solutions.List [Solutions.Elem 2; Solutions.List [Solutions.Elem 3; Solutions.Elem 4]; Solutions.Elem 5]]
        let expected = [1;2;3;4;5]
        let result = Solutions.FlattenList2 input
        result |> should equal expected

    [<Fact>]
    let ``Solution 7 collect with twist`` () =
        let input = Solutions.List [ Solutions.Elem 1; Solutions.List [Solutions.Elem 2; Solutions.List [Solutions.Elem 3; Solutions.Elem 4]; Solutions.Elem 5]]
        let expected = [1;2;3;4;5]
        let result = Solutions.FlattenList2b input
        result |> should equal expected

    [<Fact>]
    let ``Solution 7 fold`` () =
        let input = Solutions.List [ Solutions.Elem 1; Solutions.List [Solutions.Elem 2; Solutions.List [Solutions.Elem 3; Solutions.Elem 4]; Solutions.Elem 5]]
        let expected = [1;2;3;4;5]
        let result = Solutions.FlattenList3 input
        result |> should equal expected
    
    [<Fact>]
    let ``Solution 7 fold with twist`` () =
        let input = Solutions.List [ Solutions.Elem 1; Solutions.List [Solutions.Elem 2; Solutions.List [Solutions.Elem 3; Solutions.Elem 4]; Solutions.Elem 5]]
        let expected = [1;2;3;4;5]
        let result = Solutions.FlattenList3b input
        result |> should equal expected

    [<Fact>]
    let ``Solution 8 recursive`` () =
        let input = [1; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [1;2;3;2;3;2;5]
        let result = Solutions.RemoveConsecutiveDuplicates1 input
        result |> should equal expected
    
    [<Fact>]
    let ``Solution 8 fold`` () =
        let input = [1; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [1;2;3;2;3;2;5]
        let result = Solutions.RemoveConsecutiveDuplicates2 input
        result |> should equal expected

    [<Fact>]
    let ``Solution 8 fold improved`` () =
        let input = [1; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [1;2;3;2;3;2;5]
        let result = Solutions.RemoveConsecutiveDuplicates2b input
        result |> should equal expected
    
    [<Fact>]
    let ``Solution 8 foldBack`` () =
        let input = [1; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [1;2;3;2;3;2;5]
        let result = Solutions.RemoveConsecutiveDuplicates3 input
        result |> should equal expected
    
    [<Fact>]
    let ``Solution 9`` () =
        let input = [1; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [[1];[2];[3;3];[2];[3];[2;2;2];[5;5]]
        let result = Solutions.PackConsecutiveDuplicates1 input
        result |> should equal expected
    
    [<Fact>]
    let ``Solution 9 alt`` () =
        let input = [1; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [[1];[2];[3;3];[2];[3];[2;2;2];[5;5]]
        let result = Solutions.PackConsecutiveDuplicates1a input
        result |> should equal expected
    
    [<Fact>]
    let ``Solution 9 alt''`` () =
        let input = [1; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [[1];[2];[3;3];[2];[3];[2;2;2];[5;5]]
        let result = Solutions.PackConsecutiveDuplicates1a input
        result |> should equal expected

    [<Fact>]
    let ``Solution 10 map`` () =
        let input = [1; 2; 2; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [(1, 1);(3, 2);(2, 3); (1, 2); (1, 3); (3, 2); (2, 5)]
        let result = Solutions.RunLenghtEncoding1 input
        result |> should equal expected

    [<Fact>]
    let ``Solution 10 alt`` () =
        let input = [1; 2; 2; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [(1, 1);(3, 2);(2, 3); (1, 2); (1, 3); (3, 2); (2, 5)]
        let result = Solutions.RunLenghtEncoding2 input
        result |> should equal expected

    [<Fact>]
    let ``Solution 10 foldback`` () =
        let input = [1; 2; 2; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [(1, 1);(3, 2);(2, 3); (1, 2); (1, 3); (3, 2); (2, 5)]
        let result = Solutions.RunLenghtEncoding3smpl input
        result |> should equal expected
    
    [<Fact>]
    let ``Solution 10 foldback fancy`` () =
        let input = [1; 2; 2; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [(1, 1);(3, 2);(2, 3); (1, 2); (1, 3); (3, 2); (2, 5)]
        let result = Solutions.RunLenghtEncoding3fancy input
        result |> should equal expected