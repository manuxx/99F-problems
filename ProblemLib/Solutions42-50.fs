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
        let toString bs = System.String.Join(" ",Array.ofList(bs |> List.map string))
        values |> Seq.iter(fun bs-> printf "%s %b\n" (bs |> toString) (expr bs))

    let tablen n expr =
        tablengen n expr (fun n -> replicate n [true; false])

    tablen 3 (fun [a;b;c] -> a && b || (a=c))