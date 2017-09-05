module MerkleTreeTests

open Expecto

[<EntryPoint>]
let main args = 
    let result = runTestsInAssembly defaultConfig args
    System.Console.ReadLine() |> ignore
    result