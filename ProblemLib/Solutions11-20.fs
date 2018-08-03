namespace Problems_solutions

module Solutions11=
    type 'a EncodingRLE = 
    | Single of 'a
    | Multiple of int * 'a
    
    let RunLenghtEncodingMod (input:'a list) : 'a EncodingRLE list =
        input
        |> Solutions.PackConsecutiveDuplicates1
        |> List.map (fun nestedList -> 
                        if nestedList.Length = 1 then 
                            Single (List.head nestedList)
                        else
                            Multiple (List.length nestedList, List.head nestedList)
                     )

    let RunLenghtDecode (input: 'a EncodingRLE list) : 'a list  =
        input 
        |> List.map (function
                     | Single el -> [el]
                     | Multiple (cnt,el) -> List.replicate cnt el
                     )
        |> List.concat

    let RunLenghtDecodeAlt (input: 'a EncodingRLE list) : 'a list  =
        let expand = function
            | Single e -> [e]
            | Multiple(cnt, el) -> List.replicate cnt el

        List.collect expand input

    let RunLenghtEncodingDirect  (input:'a list) : 'a EncodingRLE list =
        let RLE cnt elem  = 
            if cnt=1 then Single (elem) else Multiple (cnt, elem)

        let rec encode accum cnt last restOfInput  : 'a EncodingRLE List =
            match restOfInput with
            | [] -> (RLE cnt last) :: accum
            | cur::tail when cur <> last -> encode ((RLE cnt last) :: accum) 1 cur tail
            | _::tail -> encode accum (cnt+1) last tail
        
        match input with
        | [] -> []
        | first :: tail -> encode [] 1 first tail |> List.rev

    let RunLenghtEncodingDirectAlt  (input:'a list) : 'a EncodingRLE list =
        let rec encode accum restOfInput  : 'a EncodingRLE List =
            match restOfInput with
            | [] -> accum
            | cur::input_tail -> match accum with 
                                    | [] -> encode [Single cur] input_tail
                                    | Single last :: accum_tail when last=cur -> 
                                        encode (Multiple (1+1, cur)::accum_tail) input_tail
                                    | Single _ :: _   -> 
                                        encode (Single cur::accum) input_tail
                                    | Multiple (cnt, last) :: accum_tail when last=cur -> 
                                        encode (Multiple (cnt+1, cur)::accum_tail) input_tail
                                    | Multiple (_, _) :: _  -> 
                                        encode (Single cur::accum) input_tail
        
        encode [] input |> List.rev

    let RunLenghtEncodingDirectFoldBack  (input:'a list) : 'a EncodingRLE list =
        let proc curr accum =
            match accum with
            | [] -> [Single curr]
            | Single last :: tail when last=curr -> Multiple(2, curr) :: tail
            | Single _ :: _  -> Single curr :: accum
            | Multiple (cnt,last) :: tail when last=curr -> Multiple(cnt+1, curr) :: tail
            | Multiple (_,_) :: _ -> Single curr :: accum
        List.foldBack proc input [] 

    let RunLenghtEncodingDirectFoldBackAltFun  (input:'a list) : 'a EncodingRLE list =
        let proc curr = function
            | [] -> [Single curr]
            | Single last :: tail when last=curr -> Multiple(2, curr) :: tail
            | Single _ :: _  as accum -> Single curr :: accum
            | Multiple (cnt,last) :: tail when last=curr -> Multiple(cnt+1, curr) :: tail
            | Multiple (_,_) :: _ as accum -> Single curr :: accum
        List.foldBack proc input [] 

    let DuplicateListConcat (input: 'a list) : 'a list =
        input |> List.map (fun elem -> [elem; elem]) |> List.concat

    let rec DuplicateListRecur (input:'a list): 'a list =
        match input with
            | [] -> []
            | h::tail -> h::h::(DuplicateListRecur tail)

    let DuplicateListYield (input:'a list): 'a list =
        [for x in input do yield x; yield x]
    
    let DuplicateListCollect (input: 'a list) : 'a list =
        List.collect (fun elem -> [elem; elem]) input
    
    let DuplicateListFoldBack (input: 'a list) : 'a list =
        //List.foldBack (fun elem acc -> elem::elem::acc) input [] 
        (input, []) ||> List.foldBack (fun elem acc -> elem::elem::acc)
    
    let DuplicateListFold (input: 'a list) : 'a list =
        //input |> List.fold (fun acc elem -> elem::elem::acc) [] |> List.rev 
        ([], input) ||> List.fold (fun acc elem -> acc @ [elem; elem])

    let DuplicateListCollectRepl (input: 'a list) : 'a list =
        input |> List.collect (List.replicate 2)
    
    let DuplicateListRecurAccum (input:'a list): 'a list =
        let rec workFun acc inp =
            match inp with
            | [] -> acc
            | h::tail -> workFun  (h::h::acc) tail 
        workFun [] input |> List.rev

    let MultiplyListCollectRepl (input: 'a list) times : 'a list =
        input |> List.collect (List.replicate times)
    
    let MultiplyListYield (input:'a list) times: 'a list =
        [for x in input do for cnt = 1 to times do yield x]
    
    let DropEveryNthElementRecAccum (input:'a list) n: 'a list =
        let rec work acc input i =
            match input with
            | []-> acc
            | _::tail when i>=n -> work acc tail 1
            | h::tail -> work (h::acc) tail (i+1)
        work [] input 1 |> List.rev
    
    let DropEveryNthElementFold (input:'a list) n: 'a list =
        input 
        |> List.fold (fun (acc,i) x -> if i>= n then (acc,1) else (x::acc,i+1)) ([],1)
        |> fst
        |> List.rev

    let DropEveryNthElementZipFilter (input:'a list) n: 'a list =
        List.zip input [1..input.Length] 
        |> List.filter (fun (elem,i)-> i % n<>0)
        |> List.map fst
    
    let DropEveryNthElementMapiFilter (input:'a list) n: 'a list =
        input 
        |> List.mapi (fun i elem -> (i+1,elem))
        |> List.filter (fun (i,elem)-> i % n<>0)
        |> List.map snd
        
    let DropEveryNthElementRec (input:'a list) n: 'a list =
        let rec work input i =
            match input with
            | []-> []
            | _::tail when i>=n -> work tail 1
            | h::tail -> h:: work tail (i+1)
        work input 1 
  
    let SplitList (input:'a list) n : 'a list * 'a list =
        let rec slicer acc inp i =
            match inp with 
            | [] -> (List.rev acc, [])
            | h::tail when i<=n -> slicer (h::acc) tail (i+1)
            | _::_ -> (List.rev acc, inp) 
        slicer [] input 1

    let SplitList1 (input:'a list) n : 'a list * 'a list =
        let rec slicer acc inp i =
            match inp,i with 
            | [],_ -> (List.rev acc, [])
            | _::_,0 -> (List.rev acc, inp) 
            | h::tail,i -> slicer (h::acc) tail (i-1)
        slicer [] input n
    
    let SplitList2 (input:'a list) n : 'a list * 'a list =
        let rec take acc inp i =
            match inp,i with 
            | [],_ -> List.rev acc
            | _::_,0 -> List.rev acc
            | h::tail,i -> take (h::acc) tail (i-1)
        let rec drop inp i =
            match inp,i with 
            | [],_ -> []
            | _::_,0 -> inp
            | _::tail,i -> drop tail (i-1)
        (take [] input n, drop input n)

    let SliceList (input:'a list) from_i to_i  : 'a list =
        let rec take acc i inp=
            match inp with 
            | [] -> List.rev acc
            | _::_ when i<1 -> List.rev acc
            | h::tail -> take (h::acc) (i-1) tail 
        let rec drop i inp =
            match i,inp with 
            | _,[] -> []
            | 0,_::_ -> inp
            | i,_::tail -> drop (i-1) tail 
        input
        |> drop (from_i-1)
        |> take [] (to_i-from_i+1)
    
    let SliceListZipFilter (input:'a list) from_i to_i  : 'a list =
        input 
        |> List.zip [1..input.Length]
        |> List.filter (fun (i,_) -> from_i<=i && i<=to_i)
        |> List.map snd

    let SliceListMapiFilter (input:'a list) from_i to_i  : 'a list =
        input 
        |> List.mapi  (fun i elem -> (i+1,elem))
        |> List.filter (fun (i,_) -> from_i<=i && i<=to_i)
        |> List.map snd

    let SliceListZipLazy(input:'a list) from_i to_i   =
        input 
        |> Seq.zip (seq {1..to_i}) 
        |> Seq.filter (fst >> (<=)from_i) 
        |> Seq.map snd
        |> List.ofSeq

    let SliceListYield(input:'a list) from_i to_i   =
        [ for i in Seq.zip (seq{ 1 .. to_i}) input do if (fst i)>=from_i then yield(snd i) ]
    
    let SliceListYieldAlt(input:'a list) from_i to_i   =
        [ for (i,e) in Seq.zip [ 1 .. to_i] input do if (i)>=from_i then yield e ]
    
    let LeftRotateList (input:'a list) k : 'a list   =
        let rec take acc i inp=
            match inp with 
            | [] -> List.rev acc
            | _::_ when i<1 -> List.rev acc
            | h::tail -> take (h::acc) (i-1) tail 
        let rec drop i inp =
            match i,inp with 
            | _,[] -> []
            | 0,_::_ -> inp
            | i,_::tail -> drop (i-1) tail 
        let k = k % (input.Length)
        (drop k input) @ (take [] k input)
    
    let rec LeftRotateListRec (input:'a list) k : 'a list   =
        match input,k with
        | _,0 -> input
        | [],_ -> []
        | h :: tail,k -> LeftRotateListRec (tail @ [h]) (k-1)
    
    let rec LeftRotateListSplit (input:'a list) k : 'a list   =
        let at = let ln = List.length input in abs <| (ln + k) % ln
        let st,nd = SplitList input at
        nd @ st

    let RemoveKthFromList (input:'a list) k : 'a list   =
        let rec take acc i inp=
            match inp with 
            | [] -> List.rev acc
            | _::_ when i<1 -> List.rev acc
            | h::tail -> take (h::acc) (i-1) tail 
        let rec drop i inp =
            match i,inp with 
            | _,[] -> []
            | 0,_::_ -> inp
            | i,_::tail -> drop (i-1) tail 
        (take [] (k-1) input) @ (drop k input)

    let rec RemoveKthFromListSplit (input:'a list) k : 'a list   =
        if k>List.length input then
            input
        else
            let st,nd = SplitList input (k-1)
            st @ List.tail nd
