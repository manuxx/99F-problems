namespace ProblemTests

open Problems_solutions
open Xunit
open FsUnit.Xunit
open Solutions51

module Tests51 =
    type 'a Tree =  'a Solutions51.Tree
    [<Fact(Skip="not implemented yet")>]
    let ``Solution 55`` () =
        0 |> should equal 0
    [<Fact>]
    let ``Solution 56`` () =
        let exampleNSym = Branch('x',
                        Branch('y',
                            Branch('z',
                                Empty,
                                Branch('k',Empty,Empty)
                            ),
                            Empty
                        ),
                        Branch('c',Empty,Empty)
        )
        let exampleSym =   Branch('x',
                                    Branch('y',
                                        Branch('z',
                                            Branch('k',Empty,Empty),
                                            Branch('c',Empty,Empty)
                                        ),
                                        Empty
                                    ),
                                    Branch('y',
                                        Empty,
                                        Branch('z',
                                            Branch('c',Empty,Empty),
                                            Branch('k',Empty,Empty)
                                        )
                                    )
                                )
        isSymTree exampleSym |> should equal true
        isSymTree exampleNSym |> should equal false
