module MerkleTreeTests

open Expecto
open MerkleTree

[<Tests>]
let tests = 
    testList "MerkleHash" [
        test "MerkleHash is a SHA256 hash" {
            let hash = MerkleHash.ofString "Test 1"
            let hashString = MerkleHash.toString hash
            Expect.equal hashString "26EA0AE294881F1260ECAFEC008426894E80BC4D7DC1CD6557AB9169E1A803EE" "SHA356 Hash value expected"
        }
        test "MerkleHash.ofString with same string are equal" {
            let h1 = MerkleHash.ofString "Test 1"
            let h2 = MerkleHash.ofString "Test 1"
            let h3 = MerkleHash.ofString "Test 1"
            Expect.isTrue (MerkleHash.eq h1 h2) "Hashes expected to be equal"
            Expect.isTrue (MerkleHash.eq h1 h3) "Hashes expected to be equal"
        };
        test "MerkleHash.ofString with different strings are not equal" {
            let h1 = MerkleHash.ofString "Test 1"
            let h2 = MerkleHash.ofString "Test 2"
            let h3 = MerkleHash.ofString "Test 3"
            Expect.isFalse (MerkleHash.eq h1 h2) "Hashes expected to be not equal"
            Expect.isFalse (MerkleHash.eq h1 h3) "Hashes expected to be not equal"
        };
        test "MerkleHash.concat combines two MerkleHash's hashes" {
            let h1 = MerkleHash.ofString "Test 1"
            let h2 = MerkleHash.ofString "Test 2"
            let concatHash = MerkleHash.concat h1 h2
            let hashString = MerkleHash.toString concatHash
            Expect.notEqual hashString (MerkleHash.toString h1) "Hashes expected to be equal"
            Expect.notEqual hashString (MerkleHash.toString h2) "Hashes expected to be equal"
            Expect.equal hashString "8CFFDA2813FD84A88805402993C0CAC34663E333E921E3404A86DCCA6D4C26CF" "Hashes expected to be equal"
        };
    ]

[<EntryPoint>]
let main args = 
    let result = runTestsInAssembly defaultConfig args
    System.Console.ReadLine() |> ignore
    result