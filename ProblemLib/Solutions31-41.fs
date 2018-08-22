namespace Problems_solutions

module Solutions31 =
    let isPrime num= 
        let sqrtn n = int (sqrt (float n) )
        match num with
        |1 -> true
        |2 -> true
        |3 -> true
        | num -> 
            2::[3..5..(sqrtn num)] 
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
        