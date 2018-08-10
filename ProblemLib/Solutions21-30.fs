namespace Problems_solutions

module Solutions21=
    let InsertAtList (input:'a list) k elem : 'a list =
        let before, after = Solutions11.SplitList input (k-1)
        before @ [elem] @ after

    let rec InsertAtListRec (input:'a list) k elem : 'a list =
        match input, k with 
        | _, 1 -> elem :: input
        | [], _ -> failwith "empty list!"
        | h::tail, k -> h :: InsertAtListRec tail (k-1) elem

    let InsertAtListRecAccum (input:'a list) (k:int) (elem:'a) : 'a list =
        let rec inserter acc input k elem = 
            match input, k with 
            | _, 1 -> (List.rev acc) @ (elem :: input)
            | [], _ -> failwith "empty list!"
            | h::tail, k -> inserter (h::acc) tail (k-1) elem
        inserter [] input k elem

    let RangeList from_i to_i : int list =
        //[ for i = from_i to to_i do yield i ]
        [from_i..to_i]

    let NTimePickRandomWithRepetitions (input : 'a list) (n:int) : 'a seq =
        let r = System.Random();
        seq { for i in 1..n do yield(Solutions.KthListElement2 input (r.Next(1, List.length input)) )}

    let NTimePickRandom (input : 'a list) (n:int) : 'a seq =
        let rndSeq = let r = new System.Random() in seq { while true do yield r.Next() }
        input |> Seq.zip rndSeq |> Seq.sortBy fst |> Seq.map snd |> Seq.take n

    let DifNumberSequence n max_k  =
        //let random_seq = let r = new System.Random() in seq {while true do yield (r.Next)}
        let random_seq = let r = new System.Random() in Seq.initInfinite(ignore >> r.Next)
        Seq.zip (seq {1..max_k}) random_seq
        |> Seq.sortBy snd
        |> Seq.take n
        |> Seq.map fst
        |> List.ofSeq
    
    let Permutate input  =
        let random_seq = let r = new System.Random() in Seq.initInfinite(ignore >> r.Next)
        Seq.zip input random_seq
        |> Seq.sortBy snd
        |> Seq.map fst
        |> List.ofSeq

    let PermutateWithPick input  =
        NTimePickRandom input (List.length input)
        |> List.ofSeq

    let GenCombinations (input: 'a list) (n:int) : 'a list list  =
        let rec genWork (prefix:'a list) (remaining:'a list) (k:int) : 'a list list =
            match remaining,k with 
                |[],0 -> [List.rev prefix]
                |[],_ -> []
                |h::tail, _ -> (genWork (h::prefix) tail (k-1)) @ (genWork prefix tail k)
        genWork [] input n

    let rec GenCombinationsYield (input: 'a list) (subsetLen:int) : 'a list list  =
        match input,subsetLen with
        | [],_ -> [[]]
        | xs,1 -> [for e in xs do yield [e]]
        | x::xs,n -> [  for ys in GenCombinationsYield xs (n-1) do 
                           yield x::ys
                        if List.length xs >n then 
                           yield! GenCombinationsYield xs n
                        else
                           yield xs 
                     ]
    let rec GenCombinationsYieldAlt subsetLen input  =
        let rec tails = function
            | [] -> [[]]
            | _::ys as xs -> xs::tails ys
        match input,subsetLen with
            | _,0 -> [[]]
            | xs, n ->
                [for tail in tails xs do
                    match tail with
                        | [] -> ()
                        | t::ts-> for xs' in GenCombinationsYieldAlt (n-1) ts do
                                      yield t::xs']
