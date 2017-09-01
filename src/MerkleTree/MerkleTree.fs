namespace MerkleTree

open System.Linq

type MerkleHash = byte[]

type MerkleNode = { 
    Hash: MerkleHash;
    Left: MerkleNode option;
    Right: MerkleNode option;
} with 
    member this.IsLeaf = (Option.isNone this.Left) && (Option.isNone this.Right)

type MerkleTree = {
    RootNode: MerkleNode;
    Depth: int;
    Leaves: int;
} with 
    member this.RootHash = this.RootNode.Hash 

module Constants =
    let hashLength = 32

module MerkleHash =
    let ofByteArray (buffer:byte[]) : MerkleHash = System.Security.Cryptography.SHA256.Create().ComputeHash(buffer)

    let ofString (buffer:string) = System.Text.Encoding.UTF8.GetBytes(buffer) |> ofByteArray

    let concat (hash1:MerkleHash) (hash2:MerkleHash) = Array.concat [hash1; hash2] |> ofByteArray

    let toString (mh:MerkleHash) = System.BitConverter.ToString(mh).Replace("-", "")

    let eq (hash1:MerkleHash) (hash2:MerkleHash) = hash1 = hash2 || hash1.SequenceEqual(hash2)


module MerkleNode =
    let leaf hash = { Hash = hash; Left = None; Right = None; }

    let ofNodes left right =
        let mh =
            match left, right with
            | l, Some r -> MerkleHash.concat l.Hash r.Hash
            | l, None -> l.Hash
        { Hash = mh; Left = Some left; Right = right; }

    let ofNodesTuple (left, right) = ofNodes left right

    let rec createBranchWithDepth depth leaf =
        match depth with
        | 0 -> failwith "Cannot create branch with depth 0"
        | 1 -> leaf
        | _ -> ofNodes (createBranchWithDepth (depth - 1) leaf) None

    let verifyHash node =
            match node.Left, node.Right with
            | None, None -> true
            | None, _ -> failwith "Left branch must be a node  if right branch is a node"
            | Some left, None -> MerkleHash.eq node.Hash left.Hash
            | Some left, Some right -> 
                MerkleHash.concat left.Hash right.Hash 
                |> MerkleHash.eq node.Hash

    let eq node1 node2 = MerkleHash.eq node1.Hash node2.Hash


module MerkleTree = 
    module private Append =
        type AppendAction = Handled of MerkleNode | Unhandled of MerkleNode
        type NodeType = 
            | Leaf 
            | FullInternalEdge 
            | EmptyInternalEdge of left: MerkleNode 
            | EmptyInternal of left: MerkleNode 
            | FullInternal of left:MerkleNode * right: MerkleNode

        let getNodeType node = 
            match node.Left, node.Right with
            | None, None -> Leaf
            | None, _ -> failwith "Invalid tree with only left node"
            | Some l, None -> 
                match l.IsLeaf with
                | true -> EmptyInternalEdge l
                | false -> EmptyInternal l
            | Some l, Some r ->
                match l.IsLeaf with
                | true -> FullInternalEdge
                | false -> FullInternal (l, r)

        
        let append leaf tree =
            let rec tryAppend currentHeight node =
                let nextHeight = currentHeight - 1
                let nodeType = getNodeType node
                match nodeType with
                | Leaf -> Unhandled node
                | EmptyInternalEdge l -> Handled (MerkleNode.ofNodes l (Some leaf))
                | FullInternalEdge -> Unhandled node
                | EmptyInternal l -> 
                    let action = tryAppend nextHeight l
                    match action with
                    | Handled n -> Handled (MerkleNode.ofNodes n None)
                    | Unhandled _ -> 
                        //create new path to the right that is the full height
                        let r = MerkleNode.createBranchWithDepth nextHeight leaf
                        Handled (MerkleNode.ofNodes l (Some r))
                | FullInternal (l, r) -> 
                    let action = tryAppend nextHeight r
                    //if right path changed, create new node. otherwise use old node
                    match action with
                    | Handled n -> Handled (MerkleNode.ofNodes l (Some n))
                    | Unhandled _ -> Unhandled node

            let appendResult = tryAppend tree.Depth tree.RootNode

            match appendResult with
            | Handled n -> { RootNode = n; Depth = tree.Depth; Leaves = tree.Leaves + 1; }
            | Unhandled n -> 
                let right = MerkleNode.createBranchWithDepth tree.Depth leaf
                let newRoot = MerkleNode.ofNodes n (Some right)
                { RootNode = newRoot; Depth = tree.Depth + 1; Leaves = tree.Leaves + 1; }


    let create leaves = 
        let rec loop depth nodes =
            match Seq.length nodes with
            | 1 -> { RootNode =  Seq.head nodes; Depth = depth; Leaves = (Seq.length leaves); }
            | _ -> 
                nodes
                |> Seq.chunkBySize 2
                |> Seq.map (fun chunk -> (Array.item 0 chunk, Array.tryItem 1 chunk))
                |> Seq.map MerkleNode.ofNodesTuple
                |> loop (depth + 1)
        loop 1 leaves

    let append = Append.append