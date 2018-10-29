namespace Problems_solutions

open System.Net.Sockets

module Solutions42 =
    open System.Net.Sockets

    let and' = (&&)
    let or' = (||)
    let nand a b = not <| and' a b
    let nor a b = not <| or' a b
    let xor a b = a <> b
    let equ a b = a=b
    let impl a b = compare a b |> (<>) 1
    let genBoolSeq digits =
        let pow2 = float>>(( **) 2.0)>> int
        let rec genBitBool acc digits n =  
            match n, digits with
                |_,0 -> acc
                |x,digits -> genBitBool ((x % 2 <> 0)::acc) (digits-1) (x/2)
        [ for i in 0..(pow2 digits)-1 do yield (genBitBool [] digits i)]

    let rec genBoolTable digits =
        if digits<=0 then [];
        else if digits=1 then [[false];[true]]
        else
            let ret = genBoolTable (digits-1)
            [for s in ret do yield false::s]
            @
            [for s in ret do yield true::s]

    let rec genBoolTable1 digits =
        if digits<=0 then [];
        else if digits=1 then [[true];[false]]
        else
            let ret = genBoolTable1 (digits-1)
            [for s in ret do yield true::s; yield false::s]


    let rec genBoolGrayTable digits =
        if digits<=0 then [];
        else if digits=1 then [[false];[true]]
        else
            let ret = genBoolGrayTable (digits-1)
            [for s in ret do yield false::s]
            @
            [for s in List.rev ret do yield true::s]
        
    
    let table predicate2 = 
        [ 
            for boolDigits in genBoolSeq 2 do match boolDigits with
                                                | d1::[d2] -> yield d1::d2::[predicate2 d1 d2]
                                                | _ -> ()
        ]

    let (&|) a b = xor a b
    let (^||) a b = nor a b
    let (^&&) a b = nand a b
    let (|->) a b = impl a b

    let replicate n xs =
        let rec repl acc n =
            match n with 
            | 0 -> acc
            | n-> 
                let acc' = acc |> List.collect(fun ys -> xs |> List.map(fun x -> x::ys))
                repl acc' (n-1)
        repl [[]] n


    let tablengen n expr generator =
        let values = generator n 
        values |> List.map (fun bs-> bs @ [expr bs] )

    let tablen n expr =
        tablengen n expr (fun n -> replicate n [true; false])

    let genGrayCode digits =
        genBoolGrayTable digits
        |> List.map (fun l -> l |> List.map (fun b-> if b then "1" else "0") |> List.fold (+) "" )
    
    let rec genGrayCodeRec digits =
        if digits<=0 then [];
        else if digits=1 then ["0";"1"]
        else
            let ret = genGrayCodeRec (digits-1)
            [for s in ret do yield "0" + s]
            @
            [for s in List.rev ret do yield "1"+s]
        
    let rec genGrayCodeRecAlt digits =
        if digits<=0 then [];
        else if digits=1 then ["0";"1"]
        else
            let stepPrev = genGrayCodeRec (digits-1)
            (stepPrev |> List.map ((+) "0") ) @ (stepPrev |> List.rev |> List.map ((+) "1") )
            
    
    let rec genHuffman inputChars =
        let rec buildCode huffmanTreeNodes =
            match huffmanTreeNodes |>  List.sortBy (fun (chars, cnt) -> cnt) with
            | [] -> []
            | [ (chars, len) ] ->  chars
            | (chars1, len1) :: (chars2, len2) :: tail ->  
                let allChars = (chars1 |> List.map (fun (char,code) -> (char,"0"+code))) 
                                @ (chars2 |> List.map (fun (char,code) -> (char,"1"+code)) )
                
                buildCode ( (allChars, len1+len2) :: tail )
                
        inputChars 
        |> List.map (fun (char,cnt) -> [(char,"")], cnt)
        |> buildCode
        |> List.sortBy (fun (char,_ ) -> char)
        

        //|> reduce

    // First we create a representation of the Huffman tree
    type 'a HuffmanTree = Node of int (*frecuency*) * 'a (* left *) HuffmanTree * 'a (* right *) HuffmanTree | Leaf of int * 'a (* term *)
    
    // Auxiliary function to get the frecuency
    let frecuency = function
        | Leaf (frec, _) -> frec
        | Node(frec, _, _) -> frec

    // Once we have build the Huffman tree, we can use this function to assing the codes
    // nodes to the left get a '0'. Nodes to the right get a '1'.
    let encode tree =
        let rec enc code tree cont =
            match tree with
                | Leaf (_, a) -> cont [(a, code)]
                | Node(_, lt, rt) ->
                    enc (code + "0") lt <| fun ltacc -> enc (code + "1") rt <| fun rtacc -> cont (ltacc @ rtacc)
        enc "" tree id
    // The algorithm is explained here: http://en.wikipedia.org/wiki/Huffman_coding
    // The implementation below uses lists. For better performance use a priority queue.
    // This is how it works. First we transform the list of terms and frecuencies into a list of Leafs (6).
    // Then, before anything happpens, we sort the list to place the terms with the lowest frecuency
    // at the head of the List (1) (this is where a priority queue would shine). 
    // Otherwise, we combine the first two elements into a Node with the combined frecuency of the two nodes (4). 
    // We add the node to the list and try again (5). Eventualy the list is reduced to 
    // one term and we're done constructing the tree (2). Once we have the tree, we just need to encode it (7).
    let huffman symbols =
        let rec createTree tree = 
            let xs = tree |> List.sortBy frecuency (* 1 *)
            match xs with
                | [] -> failwith "Empty list"
                | [x] -> x (* 2 *)
                | x::y::xs -> (* 3 *)
                    let ht = Node(frecuency x + frecuency y, x , y) (* 4 *)
                    createTree (ht::xs) (* 5 *)
        let ht = symbols 
                    |> List.map(fun (a,f) -> Leaf (f,a)) (* 6 *)
                    |> createTree
        encode ht |> List.sortBy(fst) (* 7 *)
