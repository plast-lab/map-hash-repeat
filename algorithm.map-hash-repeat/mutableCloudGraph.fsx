
// BEGIN PREAMBLE -- do not evaluate, for intellisense only
#r "Nessos.MBrace.Utils"
#r "Nessos.MBrace.Actors"
#r "Nessos.MBrace.Base"
#r "Nessos.MBrace.Store"
#r "Nessos.MBrace.Client"

#r "../lib/bin/Debug/Nessos.MBrace.Lib.dll"

open Nessos.MBrace.Client

type Node<'T> =  | N of 'T * List<IMutableCloudRef<Node<'T>>>  
type Graph<'T>= | G of List<IMutableCloudRef<Node<'T>>>

[<Cloud>]
let cloudNode node = 
    cloud {     
        return! MutableCloudRef.New(N(node))
    }
   
[<Cloud>]
let createCloudGraph (nodes : List<IMutableCloudRef<Node<'T>>>) = 
    cloud {
        return! MutableCloudRef.New(G(nodes))
    }

let n1 = (1,[])
let n2 = (2,[])
let n3 = (3,[])
let n4 = (4,[])
let n5 = (5,[])

// create a local-only runtime
let runtime = MBrace.InitLocal 4

let n1Ref = runtime.Run <@ cloudNode n1 @>
let n2Ref = runtime.Run <@ cloudNode n2 @>
let n3Ref = runtime.Run <@ cloudNode n3 @>
let n4Ref = runtime.Run <@ cloudNode n4 @>
let n5Ref = runtime.Run <@ cloudNode n5 @>


[<Cloud>]
let addNeighbor (node1 : IMutableCloudRef<Node<'T>>) (node2 : IMutableCloudRef<Node<'T>>) =
    cloud {
        let! n1 = MutableCloudRef.Read node1
        match n1 with
            | N(id,[]) -> 
                let! _ = MutableCloudRef.Force(node1,N(id,[node2]))
                return! MutableCloudRef.Read(node1)
            | N (id,nList) -> 
                let! _ = MutableCloudRef.Force(node1,N(id,List.append nList [node2]))
                return! MutableCloudRef.Read(node1)        
    }



//5->4
let n_5 = runtime.Run <@ addNeighbor n5Ref n4Ref @>

//3->4
let n_3 = runtime.Run <@ addNeighbor n3Ref n4Ref @>

//2->3
let n_2 = runtime.Run <@ addNeighbor n2Ref n3Ref @>

//1->3
let n_1 = runtime.Run <@ addNeighbor n1Ref n3Ref @>

//1->5
let n_1'= runtime.Run <@ addNeighbor n1Ref n5Ref @>

let result = runtime.Run <@ createCloudGraph [n1Ref;n2Ref;n3Ref;n4Ref;n5Ref] @>

/////////////////////////////PRINT/////////////////////////////////////////////
//get neighbors list from graph
[<Cloud>]
let getN (graph : IMutableCloudRef<Graph<'T>>) =   
    cloud {
        let! g = MutableCloudRef.Read(graph)
        match g with 
            | G(mutableNodes) -> return mutableNodes                                            
    }

let mutableNodes = runtime.Run <@ getN result @>

//return current's node id,neighbors
[<Cloud>]
let printN (node : IMutableCloudRef<Node<int>>) =     
    cloud {        
        let! nd = MutableCloudRef.Read node
        match nd with 
            | N(id,lst) -> return (id,lst)
    } 
    
//print all nodes and neighbors
let printGr (nodes : List<IMutableCloudRef<Node<int>>>)  =
    for i in nodes do
        let proc = runtime.CreateProcess <@ printN i @>
        let (id,lst) = proc.AwaitResult()
        printfn "Node: %d" id
        for j in lst do
            let proc' = runtime.CreateProcess <@ printN j @>
            let (id',lst') = proc'.AwaitResult()
            printfn "   Neighbor %d" id'

printGr mutableNodes
//////////////////////////////////////////////////////////////////////////
