namespace ProblemTests

open Problems_solutions
open Xunit
open FsUnit.Xunit

module Tests21 =
    let checkInsertCases fun4Test =
        let input = [1..10]
        fun4Test input 3 15 |> should equal ([1..2] @ [ 15 ] @ [3..10])
        fun4Test input 1 15 |> should equal ([ 15 ] @ [1..10])
        fun4Test input 10 15 |> should equal ([1..9] @ [ 15; 10 ])
        fun4Test input 11 15 |> should equal ([1..10] @ [ 15])


    [<Fact>]
    let ``Solution 21 Insert`` () =
        checkInsertCases Solutions21.InsertAtList 

    [<Fact>]
    let ``Solution 21 Insert Rec`` () =
        checkInsertCases Solutions21.InsertAtListRec 

    [<Fact>]
    let ``Solution 21 Insert RecAccum`` () =
        checkInsertCases Solutions21.InsertAtListRecAccum 

    [<Fact>]
    let ``Solution 22 Range`` () =
        Solutions21.RangeList 2 7 |> should equal [2..7]

    [<Fact>]
    let ``Solution 23`` () =
        let input = [1..20]
        let result = Solutions21.NTimePickRandomWithRepetitions input 5 |> List.ofSeq
        List.length result |> should equal 5
        for x in result do
            input |> should contain x
    
    [<Fact>]
    let ``Solution 23 without repetitions`` () =
        let input = [1..20]
        let result = Solutions21.NTimePickRandom input 5 |> List.ofSeq
        List.length result |> should equal 5
        for x in result do
            input |> should contain x

    [<Fact>]
    let ``Solution 24`` () =
        let numbers  = Solutions21.DifNumberSequence 5 49
        List.length numbers |> should equal 5
        
        //let setOfN = Set.ofList numbers
        //numbers.Length |> should equal setOfN.Count
        numbers |> should be unique
        for x in numbers do
            x |> should be (greaterThan 0)
            x |> should be (lessThanOrEqualTo 49)

    let checkPermutateCases fun4test = 
        let numbers  = fun4test [1..10]
        List.length numbers |> should equal 10
        
        //what about repetitions?
        numbers |> should be unique
        for x in numbers do
            x |> should be (greaterThan 0)
            x |> should be (lessThanOrEqualTo 10)
    
    [<Fact>]
    let ``Solution 25`` () =
        checkPermutateCases Solutions21.Permutate 
     
    [<Fact>]
    let ``Solution 25 with 23Pick`` () =
        checkPermutateCases Solutions21.PermutateWithPick 
    
    let factorial n = [1..n] |> List.reduce (*)
    let newton n k = if k = n then 1 else (factorial n) / (factorial k) /(factorial (n-k))

    let checkCombinationCases fun4test = 
        let combinations  = fun4test [1..10] 3
        let expectedCount =  newton 10 3
        List.length combinations |> should equal expectedCount
        
        for c in combinations do
            c |> should be unique
            List.length c |> should equal 3
            for x in c do
                x |> should be (greaterThan 0)
                x |> should be (lessThanOrEqualTo 10)
    
    [<Fact>]
    let ``Solution 26 rec`` () =
        checkCombinationCases Solutions21.GenCombinations 

    [<Fact>]
    let ``Solution 26 yield`` () =
        checkCombinationCases Solutions21.GenCombinationsYield 
    
    [<Fact>]
    let ``Solution 26 yield alt`` () =
        checkCombinationCases Solutions21.GenCombinationsYieldAlt 
    
    open NHamcrest.Core
    let containSameElements x = CustomMatcher<'a list>(sprintf "Equals %A" x, fun (y:'a list) -> 
                                        List.sort y = List.sort x)
    let lengths input =
        List.map (fun x -> List.length x) input 

    [<Fact>]
    let ``lengts toolkit should return lenghts of sublists`` () =
        lengths [[1;2;3];[1];[2;3]] |> should equal [3;1;2]
    
    let rec subSetsCalc n requiredSets =
        match  requiredSets with 
        | [] -> 1
        | h::t -> (newton n h ) * (subSetsCalc (n-h) t)
    
    let checkGenDisjointSetsCases fun4test = 
        let input = [1..5]
        let requiredSets = [1;1]

        let result  = fun4test requiredSets input
        let expectedCount =  subSetsCalc (List.length input) requiredSets
        let totalLetsLen = List.sum requiredSets

        List.length result |> should equal expectedCount
                    
        for set in result do
            List.length set |> should equal (List.length requiredSets)
            lengths set |> should equal requiredSets
                
            let concateneded = List.concat set
            List.length concateneded |> should equal totalLetsLen
            concateneded |> should be unique
    
    
    [<Fact>]
    let ``Solution 27 `` () =
        checkGenDisjointSetsCases Solutions21.GenDisjointSets 

    [<Fact>]
    let ``Solution 27 not diff `` () =
        checkGenDisjointSetsCases Solutions21.GenDisjointSetsNoDiff 

    [<Fact>]
    let ``Solution 27 not diff - alt version`` () =
        checkGenDisjointSetsCases Solutions21.GenDisjointSetsNoDiff1

    [<Fact>]
    let ``Solution 28a`` () =
        let input = ["abc";"de";"fgh";"de";"ijkl";"mn";"o"]
        let result  = Solutions21.SortByLen input
        result |> should equal ["o"; "de"; "de"; "mn"; "abc"; "fgh"; "ijkl"]

    [<Fact>]
    let ``Solution 28b`` () =
        let input = ["abc";"de";"fgh";"de";"ijkl";"mn";"o"]
        let result  = Solutions21.SortByLenFreq input
        result |> should equal ["ijkl"; "o"; "abc"; "fgh"; "de"; "de"; "mn"]

    [<Fact>]
    let ``Solution 28b alt`` () =
        let input = ["abc";"de";"fgh";"de";"ijkl";"mn";"o"]
        let result  = Solutions21.SortByLenFreqAlt input
        result |> should equal ["ijkl"; "o"; "abc"; "fgh"; "de"; "de"; "mn"]
        