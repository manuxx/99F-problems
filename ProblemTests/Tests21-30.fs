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
     
    [<Fact>]
    let ``Solution 26 rec`` () =
        let factorial n = [1..n] |> List.reduce (*)
        let combinations  = Solutions21.GenCombinations [1..10] 3
        let expectedCount =  (factorial 10) / (factorial 3) /(factorial (10-3))
        List.length combinations |> should equal expectedCount
        
        for c in combinations do
            c |> should be unique
            c.Length |> should equal 3
            for x in c do
                x |> should be (greaterThan 0)
                x |> should be (lessThanOrEqualTo 10)
    
    [<Fact>]
    let ``Solution 26 yield`` () =
        let factorial n = [1..n] |> List.reduce (*)
        let combinations  = Solutions21.GenCombinationsYield [1..10] 3
        let expectedCount =  (factorial 10) / (factorial 3) /(factorial (10-3))
        List.length combinations |> should equal expectedCount
        
        for c in combinations do
            c |> should be unique
            List.length c |> should equal 3
            for x in c do
                x |> should be (greaterThan 0)
                x |> should be (lessThanOrEqualTo 10)
    
    [<Fact>]
    let ``Solution 26 yield alt`` () =
        let factorial n = [1..n] |> List.reduce (*)
        let combinations  = Solutions21.GenCombinationsYieldAlt 3 [1..10]
        let expectedCount =  (factorial 10) / (factorial 3) /(factorial (10-3))
        List.length combinations |> should equal expectedCount
        
        for c in combinations do
            c |> should be unique
            List.length c |> should equal 3
            for x in c do
                x |> should be (greaterThan 0)
                x |> should be (lessThanOrEqualTo 10)
    