namespace Problems_solutions

module SolutionsSpeedTests=
    let GenComb1 input' n' =
        let rec genSub subResult input n=
            match input, n with
            | [], 0 -> [subResult]
            | [], _ -> []
            | x::xs, _ -> (genSub (subResult @ [x]) xs (n-1)) @ (genSub subResult xs n)
        genSub [] input' n'

    let GenComb2 input' n' =
        let rec genSub subResult input n=
            match input, n with
            | [], 0 -> [List.rev subResult]
            | [], _ -> []
            | x::xs, _ -> (genSub (x :: subResult) xs (n-1)) @ (genSub subResult xs n)
        genSub [] input' n'

    let rec GenComb3 input n =
        match input, n with
            | _, 0  -> [[]]
            | input, n when n >= List.length input -> [input]
            | x::xs, _ -> [
                            for x' in GenComb3 xs (n-1) do yield x::x'; 
                            for x' in GenComb3 xs n do yield x'
                          ]

    let rec GenComb4 input n =
        match input, n with
            | _, 0  -> [[]]
            | input, n when n >= List.length input -> [input]
            | x::xs, _ -> [
                            for x' in GenComb4 xs (n-1) do yield x::x'; 
                            yield! GenComb4 xs n 
                          ]
    
    let rec GenComb5 input n =
        match input, n with
            | [], _  -> [[]]
            | xs, 1 -> [for x' in xs do yield [x']]
            | x::xs, _ -> [
                            for x' in GenComb5 xs (n-1) do yield x::x'
                            if List.length xs = n then 
                                yield xs
                            else
                                yield! GenComb5 xs n 
                          ]

    let rec GenComb6 input n =
        let rec genTails = function
            | [] -> [[]]
            | _::xs as all -> all::(genTails xs)

        match  input, n with
            | _, 0 -> [[]]
            | xs, n ->  [
                          for tail in genTails xs do 
                            match tail with
                            | [] -> ()
                            | t::ts -> for x' in GenComb6 ts (n-1) do yield t::x'
            ]


            
(*

> let x = List.length (GenComb1 [1..23] 12);;
Real: 00:00:09.956, CPU: 00:00:09.968, GC gen0: 377, gen1: 137, gen2: 4
val x : int = 1352078

> let x = List.length (GenComb2 [1..23] 12);;
Real: 00:00:08.558, CPU: 00:00:08.421, GC gen0: 185, gen1: 94, gen2: 1
val x : int = 1352078

> let x = List.length (GenComb3 [1..23] 12);;
Real: 00:00:11.107, CPU: 00:00:12.687, GC gen0: 166, gen1: 88, gen2: 8
val x : int = 1352078

> let x = List.length (GenComb4 [1..23] 12);;
Real: 00:00:11.005, CPU: 00:00:12.812, GC gen0: 166, gen1: 80, gen2: 8
val x : int = 1352078

> let x = List.length (GenComb5 [1..23] 12);;
Real: 00:00:08.933, CPU: 00:00:09.640, GC gen0: 136, gen1: 55, gen2: 3
val x : int = 1352078

> let x = List.length (GenComb6 [1..23] 12);;
Real: 00:00:14.629, CPU: 00:00:16.640, GC gen0: 296, gen1: 130, gen2: 10
val x : int = 1352078

*)