namespace ProblemTests

open Problems_solutions
open Xunit
open FsUnit.Xunit
open Solutions51

module Tests51 =
    type 'a Tree =  'a Solutions51.Tree
    [<Fact(Skip="not implemented yet")>]
    let ``Solution 56`` () =
        0 |> should equal 0
    [<Fact>]
    let ``Solution 57`` () =
        let example = Branch('x',
                        Branch('y',
                            Branch('z',
                                Empty,
                                Branch('k',Empty,Empty)
                            ),
                            Empty
                        ),
                        Branch('c',Empty,Empty)
        )
        let exampleMirrored =   Branch('x',
                                    Branch('c',Empty,Empty),
                                    Branch('y',
                                        Empty,
                                        Branch('z',
                                            Branch('k',Empty,Empty),
                                            Empty
                                        )
                                    )
                                )
        mirrorTree example |> should equal exampleMirrored
