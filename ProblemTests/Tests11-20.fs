namespace ProblemTests

open Problems_solutions
open Xunit
open FsUnit.Xunit

module Tests11 =

    [<Fact>]
    let ``Solution 11 RLE modified encoding`` () =
        let input = [1; 2; 2; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [Solutions11.Single 1; Solutions11.Multiple (3, 2); Solutions11.Multiple (2, 3); Solutions11.Single 2; Solutions11.Single 3; Solutions11.Multiple (3, 2); Solutions11.Multiple (2, 5)]
        let result = Solutions11.RunLenghtEncodingMod input
        result |> should equal expected

    [<Fact>]
    let ``Solution 12 RLE decoding`` () =
        let input = [Solutions11.Single 1; Solutions11.Multiple (3, 2); Solutions11.Multiple (2, 3); Solutions11.Single 2; Solutions11.Single 3; Solutions11.Multiple (3, 2); Solutions11.Multiple (2, 5)]
        let expected = [1; 2; 2; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let result = Solutions11.RunLenghtDecode input
        result |> should equal expected

    [<Fact>]
    let ``Solution 12 RLE decoding alt`` () =
        let input = [Solutions11.Single 1; Solutions11.Multiple (3, 2); Solutions11.Multiple (2, 3); Solutions11.Single 2; Solutions11.Single 3; Solutions11.Multiple (3, 2); Solutions11.Multiple (2, 5)]
        let expected = [1; 2; 2; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let result = Solutions11.RunLenghtDecodeAlt input
        result |> should equal expected

    [<Fact>]
    let ``Solution 13 Direct RLE encoding`` () =
        let input = [1; 2; 2; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [Solutions11.Single 1; Solutions11.Multiple (3, 2); Solutions11.Multiple (2, 3); Solutions11.Single 2; Solutions11.Single 3; Solutions11.Multiple (3, 2); Solutions11.Multiple (2, 5)]
        let result = Solutions11.RunLenghtEncodingDirect input
        result |> should equal expected
    
    [<Fact>]
    let ``Solution 13 Direct RLE encoding Alt`` () =
        let input = [1; 2; 2; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [Solutions11.Single 1; Solutions11.Multiple (3, 2); Solutions11.Multiple (2, 3); Solutions11.Single 2; Solutions11.Single 3; Solutions11.Multiple (3, 2); Solutions11.Multiple (2, 5)]
        let result = Solutions11.RunLenghtEncodingDirectAlt input
        result |> should equal expected
    
    [<Fact>]
    let ``Solution 13 Direct RLE encoding FoldBack`` () =
        let input = [1; 2; 2; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [Solutions11.Single 1; Solutions11.Multiple (3, 2); Solutions11.Multiple (2, 3); Solutions11.Single 2; Solutions11.Single 3; Solutions11.Multiple (3, 2); Solutions11.Multiple (2, 5)]
        let result = Solutions11.RunLenghtEncodingDirectFoldBack input
        result |> should equal expected
    
    [<Fact>]
    let ``Solution 13 Direct RLE encoding FoldBackAltFun`` () =
        let input = [1; 2; 2; 2; 3; 3; 2; 3; 2; 2; 2; 5; 5]
        let expected = [Solutions11.Single 1; Solutions11.Multiple (3, 2); Solutions11.Multiple (2, 3); Solutions11.Single 2; Solutions11.Single 3; Solutions11.Multiple (3, 2); Solutions11.Multiple (2, 5)]
        let result = Solutions11.RunLenghtEncodingDirectFoldBackAltFun input
        result |> should equal expected

    [<Fact>]
    let ``Solution 14 Duplicate`` () =
        let input = [1; 2; 2; 3; 4; 4; 4]
        let expected = [1;1;2;2;2;2;3;3;4;4;4;4;4;4]
        let result = Solutions11.DuplicateList input
        result |> should equal expected