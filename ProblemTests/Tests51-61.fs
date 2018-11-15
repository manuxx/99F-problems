namespace ProblemTests

open Problems_solutions
open Xunit
open FsUnit.Xunit
open Solutions51

module Tests51 =
    type 'a Tree =  'a Solutions51.Tree
    
    let checkIsBalancedTeeListCorrect fun4Test = 
        let checkSingleSize n expectedCnt =
            let result = fun4Test n
            List.length result |> should equal expectedCnt
            result |> should be unique 
            List.forall isBalanced result |> should be True

        checkSingleSize 6 4
        checkSingleSize 15 1
        checkSingleSize 18 256

    [<Fact>]
    let ``Solution 55`` () =
        checkIsBalancedTeeListCorrect genBalTrees 
    
    [<Fact>]
    let ``Solution 55 yield`` () =
        checkIsBalancedTeeListCorrect genBalTreesYield 
    
    let checkIsSymTree fun4Test =
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
                                            Branch('g',Empty,Empty),
                                            Branch('k',Empty,Empty)
                                        )
                                    )
                                )
        fun4Test exampleSym |> should equal true
        fun4Test exampleNSym |> should equal false


    [<Fact>]
    let ``Solution 56 strict symmetry ie including values`` () =
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
        let exampleSym =  Branch('x',
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
        Solutions51.isSymTreeStrict exampleSym |> should equal true
        Solutions51.isSymTreeStrict exampleNSym |> should equal false

    [<Fact>]
    let ``Solution 56 in place`` () =
        checkIsSymTree Solutions51.isSymTreeIP

    [<Fact>]
    let ``Solution 56 alt syntax`` () =
        checkIsSymTree Solutions51.isSymTreeAlt

    [<Fact>]
    let ``Solution 57`` () =
        let expectedTree = Branch(1,Empty, 
                                    Branch(3,Branch (2, Empty, Empty),
                                             Branch (6, Branch (5, Branch (4, Empty, Empty),
                                                                   Empty),
                                                        Branch (8,Empty,
                                                                  Branch (9,Empty,Empty)))))
        [1;3;6;8;9;5;4;2] |> construct |> should equal expectedTree
    
    [<Fact>]
    let ``Solution 57 cont`` () =
        let expectedTree = Branch(1,Empty, 
                                    Branch(3,Branch (2, Empty, Empty),
                                             Branch (6, Branch (5, Branch (4, Empty, Empty),
                                                                   Empty),
                                                        Branch (8,Empty,
                                                                  Branch (9,Empty,Empty)))))
        [1;3;6;8;9;5;4;2] |> construct' |> should equal expectedTree