namespace Problems_solutions

module Solutions=
    let rec LastListElement1 (list:'a list) : 'a option=
        match list with
        | [] -> None
        | [ last ] -> Some last
        | _ :: tail -> LastListElement1 tail

    let LastListElement2 (list:'a list) : 'a   =
        list 
        |> List.rev 
        |> List.head
   
    let LastListElement3 (list:'a list) : 'a   =
        list 
        |> List.reduce (fun (_) (y) -> y)

    let rec ButLastListElement1 (list:'a list) : 'a option=
        match list with
        | [] -> None
        | [ _ ] -> None
        | gotIt :: [ _ ] -> Some gotIt
        | _ :: tail -> ButLastListElement1 tail

    let rec ButLastListElement2a (list:'a list) : 'a =
        list 
        |> List.rev 
        |> List.skip 1
        |> List.head

    let rec ButLastListElement2b (list:'a list) : 'a =
        list 
        |> List.rev 
        |> List.tail
        |> List.head

    let rec ButLastListElement3 (list:'a list) : 'a =
        let fliparg f a b = f b a
        list |> List.rev |> fliparg List.nth 1 
        
    let rec ButLastListElement4 (list:'a list) : 'a =
        list |> List.rev |> List.item 1 
    
    let rec KthListElement1 (list:'a list) (k:int): 'a option = 
        match list with
        | [] -> None
        | head :: tail -> 
            if k=1 then 
                Some head 
            else 
                KthListElement1 tail (k-1)
        
    let KthListElement2 (list:'a list) (k:int) : 'a = 
        list |> List.skip (k-1) |> List.head    
        
    let KthListElement3 (list:'a list) (k:int) : 'a = 
        //List.item k
        list |> List.item (k-1)

    let rec ListLenght1 (list:'a list) : int =
        match list with
        | [] -> 0
        | _ :: tail -> 1+ ListLenght1 tail
    
    let ListLenght2 (list:'a list) : int =
        let rec LLen (list:'a list) (acc:int) : int =
            match list with
            | [] -> acc
            | _ :: tail -> LLen tail (acc+1)
        LLen list 0
    
    let ListLenght2a (list:'a list) : int =
        let rec LLen (list:'a list) fa : int =
            match list with
            | [] -> fa()
            | _ :: tail -> LLen tail (fun () -> fa() + 1)
        LLen list (fun ()-> 0)
    
    let ListLenght3 (list:'a list) : int =
        list.Length
    
    let ListLenght4 (list:'a list) : int =
        list |> List.sumBy (fun x -> 1)

    let ListLenght5 (list:'a list) : int =
        list |> List.fold (fun cnt x -> cnt+1) 0

    let ListRev1 (list:'a list) : 'a list =
        list |> List.rev

    let ListRev2 (list:'a list) : 'a list =
        let rec LRev (input:'a list) (accum:'a list) : 'a list =
            match input with
            | [] -> accum
            | head :: tail -> LRev tail (head::accum)
        LRev list []

    let ListRev2a (list:'a list) : 'a list =
        let rec LRev (input:'a list) fa : 'a list =
            match input with
            | [] -> fa()
            | head :: tail -> LRev tail (fun ()-> head::fa())
        LRev list (fun ()->[])

    let rec ListRev3 (list:'a list) : 'a list =
        let rec rev acc = function //Pattern matching function
        | [] -> acc
        | x :: xs -> rev (x::acc) xs
        rev [] list
    
    let rec ListRev3a (list:'a list) : 'a list =
        let rec rev acc = fun xxx -> 
            match xxx with
            | [] -> acc
            | x :: xs -> rev (x::acc) xs
        rev [] list

    let rec ListRev4 (xs:'a list) : 'a list  =
        List.fold(fun (acc:'a list) (x:'a) -> x::acc) [] xs
    
    let rec IsListPalindrome (list:'a list):bool  =
        list = List.rev list
    
    type 'a NestedList = List of 'a NestedList list | Elem of 'a

    let rec FlattenList1 (input:'a NestedList):'a list  =
        match input with
        | Elem e -> [e]
        | List [] -> []
        | List (element :: tail) -> 
            match element with
            | Elem e -> e:: (FlattenList1 (List tail))
            | List list -> List.append (FlattenList1 (List list)) (FlattenList1 (List tail))

    let rec FlattenList2 (input:'a NestedList):'a list  =
        match input with
        | Elem e -> [e]
        // List list -> List.collect (fun e-> FlattenList2b e) list
        | List list -> 
            list |> List.map (fun e -> FlattenList2 e) |> List.collect (fun l->l)
    
    let FlattenList2b (input:'a NestedList):'a list  =
        let rec loop = 
            List.collect(function
                        | Elem e -> [e]
                        | List xs -> loop xs) // with warning
        loop [input]
    
    let rec FlattenList3 (input:'a NestedList):'a list  =
        match input with
        | Elem e -> [e]
        | List list -> 
            //list 
            //|> List.map (fun e -> FlattenList3 e) 
            //|> List.fold (fun l1 l2 -> List.append l1 l2) [] 
            List.fold (fun accum e2 -> List.append accum (FlattenList3 e2)) [] list
    
    let rec FlattenList3b (input:'a NestedList):'a list  =
        let rec loop acc = function
                            | Elem x -> x::acc
                            | List xs -> 
                                List.foldBack (fun x acc -> loop acc x) xs acc
        loop [] input
    
    let rec RemoveConsecutiveDuplicates1 (input:'a list) : 'a list =
        let rec workerFun accum input = 
            match input with
            | [] -> accum
            | x :: [] -> x::accum
            | e1::(e2::_  as tail1) when e1=e2 -> workerFun accum tail1
            | e::tail -> workerFun (e::accum) tail 
        List.rev (workerFun [] input)

    let rec RemoveConsecutiveDuplicates1a (input:'a list) : 'a list =
        let rec workerFun af input = 
            match input with
            | [] -> af()
            | x :: [] -> x::af()
            | e1::(e2::_  as tail1) when e1=e2 -> workerFun af tail1
            | e::tail -> workerFun (fun () -> e::af() ) tail 
        List.rev (workerFun (fun ()->[]) input)

    let rec RemoveConsecutiveDuplicates2 (input:'a list) : 'a list =
        match input with
        | [] -> []
        | first :: tail ->
            tail |> List.fold (fun (last,accum) x ->
                                        if x=last then 
                                            (last,accum) 
                                        else 
                                            (x,x::accum) ) (first,[first])
                |> snd 
                |> List.rev 

    let RemoveConsecutiveDuplicates2b (input:'a list) : 'a list =
        match input with
        | [] -> []
        | x :: xs -> List.fold (fun acc x -> if x = List.head acc then acc else x::acc) [x] xs |> List.rev
    
    let RemoveConsecutiveDuplicates3 (input:'a list) : 'a list =
        List.foldBack (fun x acc -> if acc.IsEmpty || x<>acc.Head then x::acc else acc) input []
    
    let RemoveConsecutiveDuplicates3b (input:'a list) : 'a list =
        List.foldBack (fun x acc -> if List.isEmpty acc then [x] elif x=List.head acc then acc else x::acc) input []
    
    let PackConsecutiveDuplicates1 (input:'a list) : 'a list list =
        List.foldBack (fun x acc -> 
                        match acc with
                        | [] -> [[x]]
                        | curGroup :: previousGroups -> 
                            match curGroup with
                            | [] -> [] // because of warning    
                            | curElem :: _ when curElem=x -> (x::curGroup) :: previousGroups
                            | _ :: _ -> [x] :: curGroup :: previousGroups
                       ) input []

    let PackConsecutiveDuplicates1a (input:'a list) : 'a list list =
        List.foldBack (fun x acc -> 
                        match acc with
                        | [] -> [[x]]
                        | (last::_ as lastGroup):: previousGroups when last = x -> (x::lastGroup) :: previousGroups
                        | accum -> [x] :: accum
                        ) input []

    let PackConsecutiveDuplicates2 (input:'a list) : 'a list list =
        let collect x = function
                        | (y::xs)::xss when x=y -> (x::y::xs)::xss
                        | xss -> [x]::xss
        List.foldBack collect input []
        
    let RunLenghtEncoding1 (input:'a list) : (int * 'a) list =
        let nestedList = PackConsecutiveDuplicates1 input
        nestedList |> List.map (fun l -> (List.length l, l.Head) )

    
    let RunLenghtEncoding2 (input:'a list) : (int * 'a) list =
        input
        |> PackConsecutiveDuplicates1 
        //|> List.map (Seq.countBy (fun x->x) >> Seq.head >> fun (a,b) -> b,a)
        |> List.map (Seq.countBy id >> Seq.head >> fun(a,b) -> b, a)

    let RunLenghtEncoding3fancy (input:'a list) : (int * 'a) list =
        let proc cur = function
            | [] -> [(1,cur)]
            | (cnt,elem) :: rest as accum -> 
                if cur=elem then
                    (cnt+1,elem) :: rest
                else
                    (1,cur) :: accum
        List.foldBack proc input []

    let RunLenghtEncoding3smpl (input:'a list) : (int * 'a) list =
        let proc cur acc = 
            match acc with
            | [] -> [(1,cur)]
            | (cnt,elem) :: rest as accum -> 
                if cur=elem then
                    (cnt+1,elem) :: rest
                else
                    (1,cur) :: acc
        List.foldBack proc input []