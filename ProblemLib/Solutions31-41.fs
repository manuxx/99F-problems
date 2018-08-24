namespace Problems_solutions

module Solutions31 =
    let isPrime num= 
        let sqrtn n = int (sqrt (float n) )
        match num with
        |1 -> false
        |2 -> true
        | num -> 
            2::[3..2..(sqrtn num)] 
            |> Seq.fold (fun divisorFound x -> divisorFound || num % x = 0 ) false
            |> not

    let isPrimeRec num = 
        let sqrtn n = int (sqrt (float n) )
        let rec isPrimeRecWork num divisor max =
            if num % divisor = 0 then false
            else 
                if divisor >= max then true
                else isPrimeRecWork num (divisor+2) max
            
        if num <=3 then true 
        else if num%2=0 then false
             else isPrimeRecWork num 3 (sqrtn num)
        
    let isPrimeAlt num= 
        let sqrtn n = int (sqrt (float n) )
        match num with
        |1 -> true
        |2 -> true
        | num -> 
            2::[3..2..(sqrtn num)] 
            |> Seq.exists (fun x -> num % x = 0 )
            |> not

    let rec gcd m n = 
        if n = 0 then abs m
        else gcd n (m%n)

    let areCoprime a b = 
        gcd a b = 1


    let totientPhi n =
        seq {1..n-1 }
        //|> Seq.filter (areCoprime n)
        |> Seq.filter (gcd n >> (=) 1)
        |> Seq.length
    
    let rec primeFactors n =
        let smallestDivisor = 
            seq { 2..n }
            |> Seq.find ( ((%) n) >> (=) 0)
        if smallestDivisor>=n then 
            [smallestDivisor]
        else 
            smallestDivisor::(primeFactors (n / smallestDivisor))

    let primeFactorsAlt n = 
       let sqrtn = float >> sqrt >> int
       let getDivisor x =
            let sq = sqrtn x
            seq {yield 2; yield! seq{3..2..sq }}
            |> Seq.tryFind (fun x' -> x % x' = 0)
       
       n
       |> Seq.unfold (fun x ->
                        if x = 1 then
                            None
                        else
                            match getDivisor x with
                            | Some divisor ->  Some (divisor, x / divisor)
                            | None -> Some (x , 1))
       |> List.ofSeq

    let primeFactorsMult n = 
        primeFactors n
        |> List.groupBy id
        |> List.map (fun (x, xs) -> (x, List.length xs))

    let totientPhiImpr n =
        primeFactorsMult n |> List.fold (fun acc (x,cnt) -> acc * (x-1) * pown x (cnt-1)  ) 1

    let primeRange low up = 
        seq { low .. up } 
        |> Seq.filter isPrime
        |> List.ofSeq

    let goldbach n =
        let x = 
            seq { 1..n/2 } 
            |> Seq.tryFind (fun x -> isPrime x && isPrime (n-x))

        match x with 
        | None -> None
        | Some x -> Some (x, n-x)


    let goldbachAlt n =
        let primes = primeRange 2 n |> Array.ofList
        let rec findPairSum (arr:int array) front back =
            let sum = arr.[front] + arr.[back]
            match compare sum n with
            | -1 -> findPairSum arr (front+1) back
            | 0 -> Some(arr.[front], arr.[back])
            | 1 -> findPairSum arr front (back-1)
            | _ -> failwith "not possible"
        findPairSum primes 0 (primes.Length-1)
            
    let goldbachList low high =
        seq {low..2..high} |> Seq.map goldbachAlt;

    let goldbachList' low high lim =
        goldbachList low high
        |> Seq.filter (fun pair -> match pair with | Some(n1,n2) ->  (n1>=lim) && (n2 >= lim) | None ->false  )
        |> List.ofSeq