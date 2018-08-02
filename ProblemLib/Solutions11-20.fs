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

    let DuplicateList (input: 'a list) : 'a list =
        List.collect (fun elem -> [elem; elem]) input
    
    let DuplicateListFold (input: 'a list) : 'a list =
        input
        |> List.fold (fun acc elem -> elem::elem::acc) [] 
        |> List.rev 

    let DuplicateListFoldBack (input: 'a list) : 'a list =
        List.foldBack (fun elem acc -> elem::elem::acc) input [] 
        
