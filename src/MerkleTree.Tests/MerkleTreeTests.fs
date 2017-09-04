module MerkleTreeTests

open Expecto
open MerkleTree

[<Tests>]
let tests = 
    testList "main" [
        test "Hello World" {
            Expect.equal 1 1 "1 != 2"
        }
    ]

[<EntryPoint>]
let main args = 
    let result = runTestsInAssembly defaultConfig args
    System.Console.ReadLine() |> ignore
    result