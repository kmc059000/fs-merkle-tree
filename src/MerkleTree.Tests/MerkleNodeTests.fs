module MerkleNodeTests

open Expecto
open MerkleTree

let createNode text = MerkleHash.ofString text |> MerkleNode.leaf
let node1 = createNode "Test 1"
let node2 = createNode "Test 2"

[<Tests>]
let tests = 
    testList "MerkleNode" [
        test "MerkleNode.ofNodes uses the concat of the hashes of its children" {
            let leaf1 = node1
            let leaf2 = node2

            let node = MerkleNode.ofNodes leaf1 (Some leaf2)

            let leaf1Hash = MerkleHash.toString leaf1.Hash
            let leaf2Hash = MerkleHash.toString leaf2.Hash
            let hashString = MerkleHash.toString node.Hash
                
            Expect.notEqual leaf1Hash hashString "Parent node should have different hash"
            Expect.notEqual leaf2Hash hashString "Parent node should have different hash"
            Expect.equal hashString "8CFFDA2813FD84A88805402993C0CAC34663E333E921E3404A86DCCA6D4C26CF" ""
        };
        test "MerkleNode.ofNodes has same hash of child if only 1 child" {
            let leaf1 = node1
            let node = MerkleNode.ofNodes leaf1 None

            let leaf1Hash = MerkleHash.toString leaf1.Hash
            let hashString = MerkleHash.toString node.Hash

            Expect.equal leaf1Hash hashString "Single Child and Parent should have the same hash"
        };
        test "MerkleNode.eq compares hashes" {
            let x = MerkleNode.ofNodes (node1) (Some node2)
            let b = MerkleNode.ofNodes (node1) (Some node2)

            Expect.isTrue (MerkleNode.eq x b) "Nodes created with same children result in equal nodes"
        };
        test "MerkleNode.createBranchWithDepth Creates a branch of nodes without siblings, all with the same hash as the leaf. Depth of 1." {
            let leafNode = node1

            let branch1 = MerkleNode.createBranchWithDepth 1 leafNode
            Expect.equal branch1 leafNode "1 deep branch is equal to the leaf"
        };
        test "MerkleNode.createBranchWithDepth Creates a branch of nodes without siblings, all with the same hash as the leaf. Depth of 2." {
            let leafNode = node1
            let leafHash = MerkleHash.toString leafNode.Hash

            let branch = MerkleNode.createBranchWithDepth 2 leafNode
            let hashString2 = MerkleHash.toString branch.Hash
            Expect.notEqual branch leafNode "2 deep branch is not equal to the leaf"
            Expect.equal hashString2 leafHash "Hash of parent should equal the leaf"
            Expect.equal branch.Left (Some leafNode) "Node left should be leaf"
            Expect.equal branch.Right None "Branch right should be empty"
        };
        test "MerkleNode.createBranchWithDepth Creates a branch of nodes without siblings, all with the same hash as the leaf. Depth of 3." {
            let leafNode = node1
            let leafHash = MerkleHash.toString leafNode.Hash

            let rec getLeftLeaf node =
                match node.Left with
                | Some n -> getLeftLeaf n
                | None -> Some node
                        

            let branch = MerkleNode.createBranchWithDepth 3 leafNode
            let hashString2 = MerkleHash.toString branch.Hash
            Expect.notEqual branch leafNode "2 deep branch is not equal to the leaf"
            Expect.equal hashString2 leafHash "Hash of parent should equal the leaf"
            Expect.notEqual branch.Left (Some leafNode) "Node left should not be leaf"
            Expect.equal branch.Right None "Branch right should be empty"
            Expect.equal (getLeftLeaf branch) (Some leafNode) "Deepest left node should not be leaf"
        };
    ]
    