namespace Problems_solutions

module Solutions=
    let rec LastListElement1 (list:List<'T>) : Option<'T>=
        match list with
        | [] -> None
        | [ last ] -> Some last
        | _ :: tail -> LastListElement1 tail

    let LastListElement2 (list:List<'T>) : 'T   =
        list 
        |> List.rev 
        |> List.head
   
    let LastListElement3 (list:List<'T>) : 'T   =
        list 
        |> List.reduce (fun (_) (y) -> y)

    let rec ButLastListElement1 (list:List<'T>) : Option<'T>=
        match list with
        | [] -> None
        | [ _ ] -> None
        | gotIt :: [ _ ] -> Some gotIt
        | _ :: tail -> ButLastListElement1 tail

    let rec ButLastListElement2a (list:List<'T>) : 'T =
        list 
        |> List.rev 
        |> List.skip 1
        |> List.head

    let rec ButLastListElement2b (list:List<'T>) : 'T =
        list 
        |> List.rev 
        |> List.tail
        |> List.head

    let rec ButLastListElement3 (list:List<'T>) : 'T =
        let fliparg f a b = f b a
        list |> List.rev |> fliparg List.nth 1 
        
    let rec ButLastListElement4 (list:List<'T>) : 'T =
        list |> List.rev |> List.item 1 
    
    let rec KthListElement1 (list:List<'T>) (k:int): Option<'T> = 
        match list with
        | [] -> None
        | head :: tail -> 
            if k=1 then 
                Some head 
            else 
                KthListElement1 tail (k-1)
        
    let KthListElement2 (list:List<'T>) (k:int) : 'T = 
        list |> List.skip (k-1) |> List.head    
        
    let KthListElement3 (list:List<'T>) (k:int) : 'T = 
        //List.item k
        list |> List.item (k-1)

    let rec ListLenght1 (list:List<'T>) : int =
        match list with
        | [] -> 0
        | _ :: tail -> 1+ ListLenght1 tail
    
    let ListLenght2 (list:List<'T>) : int =
        let rec LLen (list:List<'T>) (acc:int) : int =
            match list with
            | [] -> acc
            | _ :: tail -> LLen tail (acc+1)
        LLen list 0
    
    let ListLenght3 (list:List<'T>) : int =
        list.Length
    
    let ListLenght4 (list:List<'T>) : int =
        list |> List.sumBy (fun x -> 1)

    let ListRev1 (list:List<'T>) : List<'T> =
        list |> List.rev

    let ListRev2 (list:List<'T>) : List<'T> =
        let rec LRev (input:List<'T>) (accum:List<'T>) : List<'T> =
            match input with
            | [] -> accum
            | head :: tail -> LRev tail (head::accum)
        LRev list []

    let rec ListRev3 (xs:List<'T>) : List<'T> =
        let rec rev acc = function
        | [] -> acc
        | x :: xs -> rev (x::acc) xs
        rev [] xs

    let rec ListRev4 xs  =
        List.fold(fun acc x -> x::acc) [] xs
    
    let rec IsListPalindrome (list:List<'T>):bool  =
        list = List.rev list
    