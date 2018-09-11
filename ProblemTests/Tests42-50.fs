namespace ProblemTests

open Problems_solutions
open Xunit
open FsUnit.Xunit

module Tests42 =

    [<Fact>]
    let ``Solution 46`` =
        Solutions42.table Solutions42.and' |> should equal [[true; true; true]; [true; false; false]; [false; true; false]; [false; false; false]]
        Solutions42.table Solutions42.or' |> should equal [[true; true; true]; [true; false; true]; [false; true; true]; [false; false; false]]
        Solutions42.table Solutions42.xor |> should equal [[true; true; false]; [true; false; true]; [false; true; true]; [false; false; false]]
        Solutions42.table Solutions42.nand |> should equal [[true; true; false]; [true; false; true]; [false; true; true]; [false; false; true]]
        Solutions42.table Solutions42.nor |> should equal [[true; true; false]; [true; false; false]; [false; true; false]; [false; false; true]]
        Solutions42.table Solutions42.equ |> should equal [[true; true; true]; [true; false; false]; [false; true; false]; [false; false; true]]
        Solutions42.table Solutions42.impl|> should equal [[true; true; true]; [true; false; false]; [false; true; true]; [false; false; true]]

    [<Fact>]        
    let ``Solution 47`` =
        Solutions42.table (&&) |> should equal [[true; true; true]; [true; false; false]; [false; true; false]; [false; false; false]]
        Solutions42.table (||) |> should equal [[true; true; true]; [true; false; true]; [false; true; true]; [false; false; false]]
        Solutions42.table Solutions42.(&|) |> should equal [[true; true; false]; [true; false; true]; [false; true; true]; [false; false; false]]
        Solutions42.table Solutions42.(^&&) |> should equal [[true; true; false]; [true; false; true]; [false; true; true]; [false; false; true]]
        Solutions42.table Solutions42.(^||) |> should equal [[true; true; false]; [true; false; false]; [false; true; false]; [false; false; true]]
        Solutions42.table (=) |> should equal [[true; true; true]; [true; false; false]; [false; true; false]; [false; false; true]]
        Solutions42.table Solutions42.(|->)|> should equal [[true; true; true]; [true; false; false]; [false; true; true]; [false; false; true]]

    [<Fact>]       
    let ``Solution 48`` =
        Solutions42.tablen 3 (fun [a;b;c] -> a && (b || c) = a && b || a && c) |> should equal  [
                                                                                                    [True;True;True;True]
                                                                                                    [False;True;True;False]
                                                                                                    [True;False;True;True]
                                                                                                    [False;False;True;False]
                                                                                                    [True;True;False;True]
                                                                                                    [False;True;False;False]
                                                                                                    [True;False;False;False]
                                                                                                    [False;False;False;False]
                                                                                                ]