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

    [<Fact>]
    let ``Solution 31 alt`` () =
        checkIsPrime Solutions31.isPrimeAlt

    [<Fact>]
    let ``Solution 32`` () =
        Solutions31.gcd 36 63 |> should equal 9
        Solutions31.gcd (-3) (-6) |> should equal 3
        Solutions31.gcd (-3) 6 |> should equal 3

    [<Fact>]
    let ``Solution 33`` () =
        Solutions31.areCoprime 36 55 |> should be True
        Solutions31.areCoprime 36 9 |> should be False
    
    [<Fact>]
    let ``Solution 34`` () =
        Solutions31.totientPhi 10 |> should equal 4
    
    [<Fact>]
    let ``Solution 35`` () =
        Solutions31.primeFactors 315 |> should equal [3; 3; 5; 7]
    
    [<Fact>]
    let ``Solution 35 Alt`` () =
        Solutions31.primeFactorsAlt 315 |> should equal [3; 3; 5; 7]

    [<Fact>]
    let ``Solution 36`` () =
        Solutions31.primeFactorsMult 315 |> should equal [(3,2); (5,1); (7,1)]

    [<Fact>]
    let ``Solution 37`` () =
        Solutions31.totientPhiImpr 10 |> should equal 4

    [<Fact>]
    let ``Solution 39`` () =
        Solutions31.primeRange 10 20 |> should equal [11;13;17;19]

    [<Fact>]
    let ``Solution 40`` () =
        Solutions31.goldbach 28 |> should equal (Some (5,23))

    [<Fact>]
    let ``Solution 41 Alt`` () =
        Solutions31.goldbachAlt 28 |> should equal (Some (5,23))

    
    [<Fact>]
    let ``Solution 42`` () =
        Solutions31.goldbachList 10 20 
        |> List.ofSeq |> should equal [ Some (3,7); Some (5,7); Some (3,11); Some (3,13); Some (5,13); Some (3,17) ]
        Solutions31.goldbachList' 4 2000 50 |> should equal [Some(73, 919); Some(61, 1321); Some(67, 1789); Some(61, 1867)]
