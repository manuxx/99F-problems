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
        
    [<Fact>]
    let ``Solution 25`` () =
        let numbers  = Solutions21.Permutate [1..10]
        List.length numbers |> should equal 10
        
        //what about repetitions?
        numbers |> should be unique
        for x in numbers do
            x |> should be (greaterThan 0)
            x |> should be (lessThanOrEqualTo 10)
     
    [<Fact>]
    let ``Solution 25 with 23Pick`` () =
        let numbers  = Solutions21.PermutateWithPick [1..10]
        List.length numbers |> should equal 10
        
        //what about repetitions?
        numbers |> should be unique
        for x in numbers do
            x |> should be (greaterThan 0)
            x |> should be (lessThanOrEqualTo 10)
    
    let factorial n = [1..n] |> List.reduce (*)
    let newton n k = 
        if k = n then 1 else (factorial n) / (factorial k) /(factorial (n-k))

    [<Fact>]
    let ``Solution 26 rec`` () =
        let combinations  = Solutions21.GenCombinations [1..10] 3
        let expectedCount =  newton 10 3
        List.length combinations |> should equal expectedCount
        
        for c in combinations do
            c |> should be unique
            c.Length |> should equal 3
            for x in c do
                x |> should be (greaterThan 0)
                x |> should be (lessThanOrEqualTo 10)
    
    [<Fact>]
    let ``Solution 26 yield`` () =
        let combinations  = Solutions21.GenCombinationsYield [1..10] 3
        let expectedCount =  newton 10 3
        List.length combinations |> should equal expectedCount
        
        for c in combinations do
            c |> should be unique
            List.length c |> should equal 3
            for x in c do
                x |> should be (greaterThan 0)
                x |> should be (lessThanOrEqualTo 10)
    
    [<Fact>]
    let ``Solution 26 yield alt`` () =
        let combinations  = Solutions21.GenCombinationsYieldAlt 3 [1..10]
        let expectedCount =  newton 10 3
        List.length combinations |> should equal expectedCount
        
        for c in combinations do
            c |> should be unique
            List.length c |> should equal 3
            for x in c do
                x |> should be (greaterThan 0)
                x |> should be (lessThanOrEqualTo 10)
    
    open NHamcrest.Core
    let containSameElements x = CustomMatcher<'a list>(sprintf "Equals %A" x, fun (y:'a list) -> 
                                        List.sort y = List.sort x)
    let lengths input =
        List.map (fun x -> List.length x) input 

    [<Fact>]
    let ``Solution solTest `` () =
        lengths [[1;2;3];[1];[2;3]] |> should equal [3;1;2]
    
    let rec subSetsCalc n requiredSets =
        match  requiredSets with 
        | [] -> 1
        | h::t -> (newton n h ) * (subSetsCalc (n-h) t)
        
    [<Fact>]
    let ``Solution 27 `` () =
        let input = [1..5]
        let requiredSets = [1;1]

        let result  = Solutions21.GenDisjointSets requiredSets input
        let expectedCount =  subSetsCalc (List.length input) requiredSets
        let totalLetsLen = List.sum requiredSets

        List.length result |> should equal expectedCount
                    
        for set in result do
            List.length set |> should equal (List.length requiredSets)
            lengths set |> should equal requiredSets
                
            let concateneded = List.concat set
            List.length concateneded |> should equal totalLetsLen
            concateneded |> should be unique
    