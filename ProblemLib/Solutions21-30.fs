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

    let GenCombinations input n : 'a list list  =
        let rec genWork curSubSel remainingElements k =
            match remainingElements,k with 
                |_, 0 -> [List.rev curSubSel]
                |[], _ -> []
                |h::tail, _ -> (genWork (h::curSubSel) tail (k-1)) @ (genWork curSubSel tail k)
        genWork [] input n

    let rec GenCombinationsYield input subsetLen : 'a list list  =
        match input,subsetLen with
        | [],_ -> [[]]
        | xs,1 -> [for e in xs do yield [e]]
        | x::xs,n -> [  for ys in GenCombinationsYield xs (n-1) do 
                           yield x::ys
                        if List.length xs = n then 
                           yield xs 
                        else
                           yield! GenCombinationsYield xs n
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


    
    let rec GenDisjointSets subsetCounts input : 'a list list list =
        let difference (left:list< 'a >) (right:list< 'a >) =
            let cache = System.Collections.Generic.HashSet< 'a >(right, HashIdentity.Structural)
            left |> List.filter (fun n -> not (cache.Contains n))
        
        match subsetCounts with
        | [] -> []
        | [c] -> [
                    for ret in GenCombinations input c do
                        yield [ret]
                 ]
        | c::cs -> [
                    for group in GenCombinations input c do
                        let unused = difference input group
                        for rest in GenDisjointSets cs unused do
                            yield group::rest
                  ]

    let rec GenDisjointSetsNoDiff subsetCounts input : 'a list list list =
        let rec combinations partialResult skipped input n=
            match input,n with
                |input, 0 -> [(partialResult,input @ skipped)]
                |[], _ -> []
                |x::xs, n -> 
                    let p1 = combinations (x :: partialResult) skipped xs (n-1)
                    let p2 = combinations partialResult (x::skipped) xs n
                    p1 @ p2
        match subsetCounts with
        (* this ie an equvalen but a bit slower and redundant
        | [] -> []
        | [c] -> [
                    for ret, _ in combinations [] [] input c do
                        yield [ret]
                 ]
         *)
        | [] -> [[]]
        | c::cs -> [
                    for group, unused in combinations [] [] input c do
                        for rest in GenDisjointSetsNoDiff cs unused do
                            yield group::rest
                  ]

    let rec GenDisjointSetsNoDiff1 ns xs : 'a list list list =
        let rec combinations xs n =
            match xs, n with
                |xs, 0 -> [([],xs)]
                |[], _ -> []
                |x::xs, n -> 
                    let ts = [ for ys, zs in combinations xs (n-1) do yield (x::ys, zs)]
                    let ds = [ for ys, zs in combinations xs n do yield (ys, x::zs)]
                    ts @ ds
        match ns, xs with
        | [], _ -> [[]]
        | n::ns,xs -> [
                        for g,rs in combinations xs n do
                            for gs in GenDisjointSetsNoDiff1 ns rs do 
                                yield g::gs
                    ]

    