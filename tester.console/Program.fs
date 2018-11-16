open System
open Problems_solutions
open Solutions51
open Solutions21
open SolutionsSpeedTests

[<EntryPoint>]
let main argv =
    
    let size = if (Array.length argv)>0 then int (argv.[0]) else 15   
    let n = if (Array.length argv)>1 then int (argv.[1]) else 3   
    
    let initislNodes = [20;1;3;6;8;9;5;4;2;31;29;26;24;23;27;28;30]
    let nodes = applyFunNTimes doublesymTreeNodes size initislNodes
    let methods = [("construct",construct); ("construct'",construct'); ("construct''",construct'')]
    let planOfTests = [ for i in [1..n] do yield! Permutate methods ]
    
    let aggregatedResults = 
        let testExecute inputData (description,func4test) = 
            let stopWatch = System.Diagnostics.Stopwatch.StartNew()
            inputData |> func4test |> ignore
            stopWatch.Stop()
            let t = stopWatch.Elapsed.TotalMilliseconds
            printfn "%s - %f" description t
            (description, t)

        planOfTests
        |> List.map (testExecute nodes)
        |> List.groupBy fst
        |> List.map (fun (key, list) -> (key, (List.averageBy snd list ) ) )
        |> List.sortBy fst
    printfn "\nAverrage results of %d runs:\n%A" n aggregatedResults
    0
