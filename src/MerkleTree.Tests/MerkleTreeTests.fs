module MerkleTreeTests

open Expecto
open MerkleTree

let createNode (id:int) = 
    let text = id.ToString()
    MerkleNode.leaf (MerkleHash.ofString text) text
let createParent left right = MerkleNode.ofNodes left (Some right)

let createTree leafCount = 
    [for i in 1..leafCount do yield (createNode i)]
    |> MerkleTree.MerkleTree.create

type Direction = L | R
let rec getNodeAtPath directions node =
    match directions with
    | [] -> node
    | x::xs -> 
        match x, node.Left, node.Right with
        | L, Some n, _ -> getNodeAtPath xs n
        | R, _, Some n -> getNodeAtPath xs n
        | _ -> failwith "Could not navigate to given path"

let expectDepth depth tree =
    Expect.equal tree.Depth depth "Depth test"

let expectTreeHash (tree:MerkleTree) node = Expect.equal (MerkleHash.toString tree.RootHash) (MerkleHash.toString node.Hash) "Node equals tree hash"
let expectTreeHashNotEqual (tree:MerkleTree) node = Expect.notEqual (MerkleHash.toString tree.RootHash) (MerkleHash.toString node.Hash) "Node does not equals tree hash"

let expectHashAtPath node path tree =
    let actual = getNodeAtPath path tree.RootNode
    Expect.equal (MerkleHash.toString node.Hash) (MerkleHash.toString actual.Hash) "Expected hash at path to equal"

[<Tests>]
let tests = 
    testList "MerkleTree" [
        testList "MerkleTree.create" [
            test "with 1 leaf returns tree" {
                let leaf = (createNode 1)
                let leaves = [leaf]
                let tree = MerkleTree.MerkleTree.create leaves

                Expect.equal tree.Depth 1 "Depth expected to be 1"
                Expect.equal tree.RootHash leaf.Hash "Hash is same as leaf"
                Expect.equal tree.RootNode leaf "Root is same as leaf"
            };

            test "with 5 leaf returns tree" {
                (*
                         12345
	                  /         \
	                1234        5''
	                /  \         |
                   12   34       5'
                  / \   / \      |
                 1   2 3   4     5
                 *)

                let tree = createTree 5

                let leaf1 = getNodeAtPath [L; L; L;] tree.RootNode
                let leaf2 = getNodeAtPath [L; L; R;] tree.RootNode
                let leaf3 = getNodeAtPath [L; R; L;] tree.RootNode
                let leaf4 = getNodeAtPath [L; R; R;] tree.RootNode
                let leaf5 = getNodeAtPath [R; L; L;] tree.RootNode

                Expect.equal tree.Depth 4 "Depth expected to be 4"

                Expect.equal leaf1.Hash (createNode 1).Hash "Leaf 1 equals"
                Expect.equal leaf2.Hash (createNode 2).Hash "Leaf 2 equals"
                Expect.equal leaf3.Hash (createNode 3).Hash "Leaf 3 equals"
                Expect.equal leaf4.Hash (createNode 4).Hash "Leaf 4 equals"
                Expect.equal leaf5.Hash (createNode 5).Hash "Leaf 5 equals"

                let n12 = getNodeAtPath [L; L;] tree.RootNode
                let n34 = getNodeAtPath [L; R;] tree.RootNode
                let n5' = getNodeAtPath [R; L;] tree.RootNode

                Expect.equal n12.Hash (createParent leaf1 leaf2).Hash "Node 12 equals"
                Expect.equal n34.Hash (createParent leaf3 leaf4).Hash "Node 34 equals"
                Expect.equal n5'.Hash leaf5.Hash "Node 5' equals"

                let n1234 = getNodeAtPath [L] tree.RootNode
                let n5'' = getNodeAtPath [R] tree.RootNode

                Expect.equal n1234.Hash (createParent n12 n34).Hash "Node 1234 equals"
                Expect.equal n5''.Hash leaf5.Hash "Node 5'' equals"

                Expect.equal tree.RootNode.Hash (createParent n1234 n5'').Hash "Node 12345 equals"
            };
        ];
        testList "MerkleTree.append" [
            test "append adds to tree" {
                let tree1 = createTree 1

                let n1 = (createNode 1)
                expectDepth 1 tree1
                expectTreeHash tree1 n1

                let n2 = (createNode 2)
                let n12 = createParent n1 n2
                let tree12 = MerkleTree.append n2 tree1
                expectDepth 2 tree12
                expectTreeHash tree12 n12
                expectTreeHashNotEqual tree12 n1
                expectTreeHashNotEqual tree12 n2

                let n3 = (createNode 3)
                let n123 = createParent n12 n3
                let tree123 = MerkleTree.append n3 tree12
                expectDepth 3 tree123
                expectTreeHash tree123 n123
                expectTreeHashNotEqual tree123 n1
                expectTreeHashNotEqual tree123 n3

                let n4 = (createNode 4)
                let n34 = createParent n3 n4
                let n1234 = createParent n12 n34
                let tree1234 = MerkleTree.append n4 tree123
                expectDepth 3 tree1234
                expectTreeHash tree1234 n1234
                expectTreeHashNotEqual tree1234 n1
                expectTreeHashNotEqual tree1234 n4

                let n5 = (createNode 5)
                let n12345 = createParent n1234 n5
                let tree12345 = MerkleTree.append n5 tree1234
                expectDepth 4 tree12345
                expectTreeHash tree12345 n12345
                expectTreeHashNotEqual tree12345 n1
                expectTreeHashNotEqual tree12345 n5
                expectHashAtPath n5 [R; L; L;] tree12345

                let n6 = (createNode 6)
                let n56 = createParent n5 n6
                let n123456 = createParent n1234 n56
                let tree123456 = MerkleTree.append n6 tree12345
                expectDepth 4 tree123456
                expectHashAtPath n56 [R; L;] tree123456
                expectHashAtPath n56 [R;] tree123456
                expectHashAtPath n1234 [L;] tree123456
                expectHashAtPath n123456 [] tree123456
                expectTreeHash tree123456 n123456
                expectTreeHashNotEqual tree123456 n1
                expectTreeHashNotEqual tree123456 n6

                let n7 = (createNode 7)
                let n567 = createParent n56 n7
                let n1234567 = createParent n1234 n567
                let tree1234567 = MerkleTree.append n7 tree123456
                expectDepth 4 tree1234567
                expectTreeHash tree1234567 n1234567
                expectTreeHashNotEqual tree1234567 n1
                expectTreeHashNotEqual tree1234567 n7

                let n8 = (createNode 8)
                let n78 = createParent n7 n8
                let n5678 = createParent n56 n78
                let n12345678 = createParent n1234 n5678
                let tree12345678 = MerkleTree.append n8 tree1234567
                expectDepth 4 tree12345678
                expectTreeHash tree12345678 n12345678
                expectTreeHashNotEqual tree12345678 n1
                expectTreeHashNotEqual tree12345678 n8

                let n9 = (createNode 8)
                let n123456789 = createParent n12345678 n9
                let tree123456789 = MerkleTree.append n9 tree12345678
                expectDepth 5 tree123456789
                expectTreeHash tree123456789 n123456789
                expectTreeHashNotEqual tree123456789 n1
                expectTreeHashNotEqual tree123456789 n9
            }
        ]
    ]
    