namespace Problems_solutions

module Solutions51 =
    let x = 10
    
    type 'a Tree = Empty | Branch of 'a * 'a Tree * 'a Tree
    let isSymTreeStrict tree =
        let rec mirrorTree tree  =
            match tree with
            | Empty -> Empty
            | Branch (value, lNode, rNode) -> Branch(value, mirrorTree(rNode), mirrorTree(lNode))
        tree = mirrorTree tree

    let isSymTreeIP tree =
        let rec checkTrees tree1 tree2  =
            match tree1, tree2 with
            | (Empty,Empty) -> true
            | (Branch (val1, l1, r1), Branch(val2, l2, r2)) -> 
                                checkTrees l1 r2 && checkTrees l2 r1
            | _ -> false
        match tree with
        | Empty -> true
        | Branch(_,left,right) -> checkTrees left right

    let isSymTreeAlt tree =
        let rec mirror tree1 tree2 cont =
            match tree1,tree2 with
            | Empty, Empty -> cont true
            | Empty, Branch _ -> cont false
            | Branch _, Empty -> cont false
            | (Branch (_, l1, r1), Branch(_, l2, r2)) -> 
                    mirror l1 r2 (fun isMirrorLeft -> 
                        mirror l2 r1 (fun isMirrorRight -> cont(isMirrorLeft && isMirrorRight )))
        match tree with
        | Empty -> true
        | Branch(_,left,right) -> mirror left right id

    let rec addBST tree element =
        match tree with
        |    Empty -> Branch (element, Empty, Empty)
        |    Branch (value, ln, rn) when element < value -> Branch(value, (addBST ln element), rn)
        |    Branch (value, ln, rn)  -> Branch(value, ln, (addBST rn element))

    let construct elements =
        elements |> List.fold (fun tree element -> addBST tree element) Empty 

    //[1;3;6;8;9;5;4;2] |> construct