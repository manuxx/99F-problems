namespace Problems_solutions

module Solutions51 =
    type 'a Tree = Empty | Branch of 'a * 'a Tree * 'a Tree
    type balancedResult = 
            | Balanced of int
            | Unbalanced
    let isBalanced tree =
        let rec calcBalance tree =
            match tree with
            | Empty -> Balanced 0
            | Branch (_, t1, t2)  -> 
                match (calcBalance t1, calcBalance t2) with
                | (Unbalanced, _) -> Unbalanced
                | (_, Unbalanced) -> Unbalanced
                | (Balanced n1, Balanced n2) when abs(n1-n2)<=1 -> Balanced (n1+n2)
                |  _ -> Unbalanced 
        match (calcBalance tree) with
        | Balanced _ -> true
        | Unbalanced -> false

    let rec genBalTrees n =
        let genAllPairs l1 l2 =
            l1
            |> (List.map (fun e1 -> l2 |> List.map (fun e2 -> (e1,e2))) )
            |> List.collect id

        if n=0 then 
            [Empty]
        elif n=1 then
            [Branch ('x', Empty, Empty)]
        else
            if (n-1) % 2 = 0 then
                let treesList = genBalTrees ((n-1)/2)
                genAllPairs treesList treesList
                |> List.map (fun (t1,t2) -> Branch ('x', t1, t2))
            else
                let treesList1 = genBalTrees ((n-1)/2)
                let treesList2 = genBalTrees (1+(n-1)/2)
                genAllPairs treesList1 treesList2
                |> List.map (fun (t1,t2) -> [Branch ('x', t1, t2);Branch ('x', t2, t1)] )
                |> List.collect id

    let rec genBalTreesYield n =
        match n with
        | 0 -> [Empty]
        | n->   let q,r = let x = n-1 in x/2, x%2
                [ for i=q to q+r do
                    for lt in genBalTreesYield i do 
                        for rt in genBalTreesYield (n-1-i) do
                            yield Branch('x', lt, rt) ]

    let nodes t = 
        let rec nodes' t cont =
            match t with
            | Empty -> cont 0
            | Branch (_,lt,rt) -> nodes' lt (fun nlt -> nodes' rt (fun nrt -> cont(1+nlt+nrt)))
        nodes' t id
    
    let rec allTrees n =
        match n with
        | 0 -> [Empty]
        | n -> [ for i=0 to n-1 do
                    for lt in allTrees i do
                        for rt in allTrees (n-1-i) do
                            yield Branch ('x', lt, rt) ]

    let genBalTreesCont n = allTrees n |> List.filter (fun t-> 
                                                    match t with 
                                                    | Empty -> true
                                                    | Branch (_,lt,rt) -> abs(nodes lt - nodes rt) <= 1 )

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
            | (Branch (_, l1, r1), Branch(_, l2, r2)) -> 
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

    let rec insertBST tree element =
        match tree with
        |    Empty -> Branch (element, Empty, Empty)
        |    Branch (value, ln, rn) when element < value -> Branch(value, (insertBST ln element), rn)
        |    Branch (value, ln, rn)  -> Branch(value, ln, (insertBST rn element))

    let insertBSTAlt x tree = 
        let rec insert' t cont = 
            match t with
            | Empty -> cont <| Branch (x, Empty, Empty)
            | Branch(y, lt, rt) as t -> 
                if x<y then
                    insert' lt <| fun lt' -> cont <| Branch(y,lt', rt)
                elif x>y then
                    insert' rt <| fun rt' -> cont <| Branch(y, lt, rt')
                else
                    t
        insert' tree id

    let insertBSTAlt' x tree = 
        let rec insert' t cont = 
            match t with
            | Empty -> cont (Branch (x, Empty, Empty))
            | Branch(y, lt, rt) as t -> 
                if x<y then
                    insert' lt (fun lt' -> cont ( Branch(y,lt', rt)))
                elif x>y then
                    insert' rt (fun rt' -> cont ( Branch(y, lt, rt')))
                else
                    t
        insert' tree id

    let construct xs = xs |> List.fold (fun tree x -> insertBST tree x) Empty 
    let construct' xs = xs |> List.fold (fun tree x -> insertBSTAlt x tree) Empty
    let construct'' xs = xs |> List.fold (fun tree x -> insertBSTAlt' x tree) Empty

    (*
    let list1 = [20;1;3;6;8;9;5;4;2;31;29;26;24;23;27;28;30]
    let doublesymTreeNodes list = 
        let max1 = List.max list
        ((max1+1) :: list) @ (List.map (fun x -> 2*max1 + 2 - x) list)
    
    let applyFunNTimes f n x = 
        [1..n] |> List.fold (fun l i -> f l ) x 

    let nodes = applyFunNTimes doublesymTreeNodes 15 list1
        
    nodes |> construct |> ignore
    nodes |> construct' |> ignore
    nodes |> construct'' |> ignore

    let tree = nodes |> construct 
    printf "isSymTreeIP %b" (isSymTreeIP tree)
    printf "isSymTreeAlt %b" (isSymTreeAlt tree)
    *)