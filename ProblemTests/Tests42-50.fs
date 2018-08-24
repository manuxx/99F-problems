namespace ProblemTests

open Problems_solutions
open Xunit
open FsUnit.Xunit

module Tests42 =
    [<Fact>]
    let ``Solution 46 rec`` =
        Solutions42.table Solutions42.and' |> should equal [[true; true; true]; [true; false; false]; [false; true; false]; [false; false; false]]
        Solutions42.table Solutions42.or' |> should equal [[true; true; true]; [true; false; true]; [false; true; true]; [false; false; false]]
        Solutions42.table Solutions42.xor |> should equal [[true; true; false]; [true; false; true]; [false; true; true]; [false; false; false]]
        Solutions42.table Solutions42.nand |> should equal [[true; true; false]; [true; false; true]; [false; true; true]; [false; false; true]]
        Solutions42.table Solutions42.nor |> should equal [[true; true; false]; [true; false; false]; [false; true; false]; [false; false; true]]
        Solutions42.table Solutions42.equ |> should equal [[true; true; true]; [true; false; false]; [false; true; false]; [false; false; true]]
        Solutions42.table Solutions42.impl|> should equal [[true; true; true]; [true; false; false]; [false; true; true]; [false; false; true]]
        
        