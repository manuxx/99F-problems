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
    let ``Solution 5 fold`` () =
        let list = ["abc"; "def"; "xxx"]
        let len = Solutions.ListRev4 list
        len |> should equal ["xxx"; "def"; "abc"]
    
    [<Fact>]
    let ``Solution 6`` () =
        let list = ["a"; "b"; "a"]
        let isPalindrome = Solutions.IsListPalindrome list
        isPalindrome |> should be True