namespace Problems_solutions

module Solutions51 =
    let x = 10
    
    type 'a Tree = Empty | Branch of 'a * 'a Tree * 'a Tree
    let rec mirrorTree tree  =
        match tree with
        | Empty -> Empty
        | Branch (value, lNode, rNode) -> Branch(value, mirrorTree(rNode), mirrorTree(lNode))

