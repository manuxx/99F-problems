namespace Problems_solutions

module Solutions21=
    let InsertAtList (input:'a list) k elem : 'a list =
        let before, after = Solutions11.SplitList input (k-1)
        before @ [elem] @ after