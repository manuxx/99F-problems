namespace Problems_solutions

module Solutions42 =
    let and' = (&&)
    let or' = (||)
    let nand a b = not <| and' a b
    let nor a b = not <| or' a b
    let xor a b = a <> b
    let equ a b = a=b
    let impl a b = compare a b |> (<>) 1

    let table predicate2 = 
        let pow2 = float>>(( **) 2.0)>> int
        let rec genBitBool acc digits n =  
            match n, digits with
                |_,0 -> acc
                |x,digits -> genBitBool ((x % 2 <> 0)::acc) (digits-1) (x/2)
        let genBoolSeq digits =
            [ for i in 0..(pow2 digits)-1 do yield (genBitBool [] digits i)]
            
        [ 
            for boolDigits in genBoolSeq 2 do match boolDigits with
                                                | d1::[d2] -> yield d1::d2::[predicate2 d1 d2]
                                                | _ -> ()
        ]