namespace ProblemTests

open Problems_solutions
open Xunit
open FsUnit.Xunit

module Tests21 =
    let checkInsertCases fun4Test =
        let input = [1..10]
        fun4Test input 5 15 |> should equal ([1..4] @ [ 15 ] @ [5..10])

    [<Fact>]
    let ``Solution 11 RLE modified encoding`` () =
        checkInsertCases Solutions21.InsertAtList 
