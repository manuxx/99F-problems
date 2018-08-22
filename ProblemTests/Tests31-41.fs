namespace ProblemTests

open Problems_solutions
open Xunit
open FsUnit.Xunit

module Tests31 =
    let checkIsPrime fun4Test =
        fun4Test 2 |> should be True
        fun4Test 3 |> should be True
        fun4Test 4 |> should be False
        fun4Test 5 |> should be True
        fun4Test 1005 |> should be False
        fun4Test 9157 |> should be True

    [<Fact>]
    let ``Solution 31`` () =
        checkIsPrime Solutions31.isPrime 

    [<Fact>]
    let ``Solution 31 rec`` () =
        checkIsPrime Solutions31.isPrimeRec 

    