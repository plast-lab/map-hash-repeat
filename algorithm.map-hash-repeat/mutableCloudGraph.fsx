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
let createCloudGraph (nodes : IMutableCloudRef<Node<'T>> list) = 
    cloud {
        return! MutableCloudRef.New(G(nodes))
    }

[<Cloud>]
let rec seqMap (f : 'T -> ICloud<'S>) (inputs : 'T list) : ICloud<'S list> =
    cloud {
        match inputs with
        | [] -> return []
        | x :: xs ->
            let! v = f x
            let! vs = seqMap f xs
            return v :: vs
    }
(*
//bug
[<Cloud>]
let rec sequence (list:  ICloud<'T> list) : ICloud<'T list> = 
    cloud {   
        match list with 
            | [] -> return List.empty
            | x::xs -> 
                let! v = x
                let! vs = sequence xs
                return v :: vs
    }
*)

//initialize graph with num nodes
[<Cloud>]
let initCloudGraph num = 
    cloud {
        let! res = [| for i in 1..num ->  (i, []) |] |> Array.toList |> seqMap cloudNode
        return! createCloudGraph res
    }

// create a local-only runtime
let runtime = MBrace.InitLocal 4

let graph = runtime.Run <@ initCloudGraph 5 @>

//returns all nodes in a list
[<Cloud>]
let printGraph (graph : IMutableCloudRef<Graph<'T>>) = 
    let getNode (nodeRef : IMutableCloudRef<Node<'T>>) : ICloud<Node<'T>>= 
        cloud {
            return! MutableCloudRef.Read(nodeRef)        
        }
    cloud   {       
        let! g = MutableCloudRef.Read(graph)
        match g with
            | G(nodes) ->                                                                                   
                return! seqMap getNode nodes
    }   
    
runtime.Run <@ printGraph graph @>

//returns an array with process - reference of each node
[<Cloud>]
let getNodeRefs (graph : IMutableCloudRef<Graph<'T>>) = 
    cloud {
        let! gr = MutableCloudRef.Read(graph)
        match gr with
            | G(nodes) ->
                return [|for i in nodes -> i |]
    }

runtime.Run <@ getNodeRefs graph @>


//calculate the power of the id of each node (in parallel) and then sum the result 
[<Cloud>]
let powerSumParallel (graph : IMutableCloudRef<Graph<'T>>) = 
    cloud   {       
        let! g = MutableCloudRef.Read(graph)
        match g with
            | G(nodes) ->                                                           
                let power (x : IMutableCloudRef<Node<'T>>)  =
                    cloud {
                        let! node = MutableCloudRef.Read(x)
                        match node with
                            | N(id,nl) ->
                                do! Cloud.OfAsync <| Async.Sleep 3000
                                return id*id
                    }                              
                let jobs = [| for node in nodes -> power node |]   
                let! results = Cloud.Parallel jobs   
                return Array.sum results
    }

#time
runtime.Run <@ powerSumParallel graph @>

//calculate the power of the id of each node (sequential) and then sum the result 
[<Cloud>]
let powerSumSequential (graph : IMutableCloudRef<Graph<'T>>) = 
    cloud   {       
        let! g = MutableCloudRef.Read(graph)
        match g with
            | G(nodes) ->                              
                let power (x : IMutableCloudRef<Node<'T>>)  =
                    cloud {
                        let! node = MutableCloudRef.Read(x)
                        match node with
                            | N(id,nl) ->
                                do! Cloud.OfAsync <| Async.Sleep 3000
                                return id*id
                    }                                        
                let! results = seqMap power nodes
                return List.sum results
    }

runtime.Run <@ powerSumSequential graph @>


//add a new node (id,neighborsList) in graph
[<Cloud>]
let addToGraph (node :('T*'a list)) (graph : IMutableCloudRef<Graph<'T>>) = 
    cloud {
        let! nodeRef = cloudNode node
        let! g = MutableCloudRef.Read(graph)
        match g with 
            | G(nodes) ->
                let n = List.append nodes [nodeRef]
                let! _ =  MutableCloudRef.Force(graph,G(n))
                return graph
    }

runtime.Run <@ addToGraph (6,[]) graph @>

//adds node at node2Pos to the neighbors list at the node in node1Pos
[<Cloud>]
let addNeighbor (node1Pos : int) (node2Pos : int) (graph : IMutableCloudRef<Graph<'T>>) = 
    cloud {                      
        let! g = MutableCloudRef.Read(graph)
        match g with 
            | G(nodes) ->               
                let node1 = nodes.[node1Pos]
                let! n1 = MutableCloudRef.Read(node1) 
                let node2 = nodes.[node2Pos]
                match n1 with
                    | N(id,list) ->
                        let neighbors = List.append list [node2]
                        let! _ = MutableCloudRef.Force(node1,N(id,neighbors))
                        return graph            
    }

runtime.Run <@ addNeighbor 0 1 graph @>
runtime.Run <@ addNeighbor 1 3 graph @>
runtime.Run <@ addNeighbor 1 2 graph @>
runtime.Run <@ addNeighbor 2 4 graph @>
runtime.Run <@ addNeighbor 2 0 graph @>
runtime.Run <@ addNeighbor 3 0 graph @>
runtime.Run <@ addNeighbor 3 2 graph @>


//calculate the power of the id of each neighbor and the result is the sum of the powers of every node
[<Cloud>]
let sumNeighbors (graph : IMutableCloudRef<Graph<'T>>) = 
    cloud   {       
        let! g = MutableCloudRef.Read(graph)
        match g with
            | G(nodes) ->                     
                let myPower (n : IMutableCloudRef<Node<'T>> ) =
                    cloud {
                        let! node = MutableCloudRef.Read(n)
                        match node with
                            | N(id,nList) ->
                                let power (x : IMutableCloudRef<Node<'T>>)  =
                                    cloud {
                                        let! node = MutableCloudRef.Read(x)
                                        match node with
                                            | N(id,nl) ->
                                                do! Cloud.OfAsync <| Async.Sleep 3000
                                                return id*id
                                    }                              
                                return! seqMap power nList
                    }    
                let jobs = [| for node in nodes -> myPower node|]
                let! results = Cloud.Parallel jobs   
                //let! results = seqMap myPower nodes     //seq
                return [|for i in results -> List.sum i|]
    }

runtime.Run <@ sumNeighbors graph @>







///////////////////////////////////////////////////////////////////////////////////////////////
[<Cloud>]
let addNeighbor1 (node1 : IMutableCloudRef<Node<'T>>) (node2 : IMutableCloudRef<Node<'T>>) =
    cloud {
        let! n1 = MutableCloudRef.Read node1
        match n1 with
            | N(id,[]) -> 
                let! _ = MutableCloudRef.Force(node1, N(id,[node2]))
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


//get neighbors list from graph
[<Cloud>]
let getN (graph : IMutableCloudRef<Graph<'T>>) =   
    cloud {
        let! g = MutableCloudRef.Read(graph)
        match g with 
            | G(mutableNodes) -> return mutableNodes                                            
    }

let mutableNodes = runtime.Run <@ getN result @>

/////////////////////////////PRINT/////////////////////////////////////////////

//return current's node id,neighbors
[<Cloud>]
let getData (node : IMutableCloudRef<Node<'T>>) =     
    cloud {        
        let! nd = MutableCloudRef.Read node
        match nd with 
            | N(id,lst) -> return (id,lst)
    } 
    
//print all nodes and neighbors
let printGr (nodes : List<IMutableCloudRef<Node<'T>>>)  =
    for i in nodes do
        let proc = runtime.CreateProcess <@ getData i @>
        let (id,lst) = proc.AwaitResult()
        printfn "Node: %d" id
        for j in lst do
            let proc' = runtime.CreateProcess <@ getData j @>
            let (id',lst') = proc'.AwaitResult()
            printfn "   Neighbor %d" id'

printGr mutableNodes
//////////////////////////////////////////////////////////////////////////

//////////////////////////////SUM//////////////////////////////////////////
let sum (nodes : List<IMutableCloudRef<Node<'T>>>) =     
    let sums = ref List.empty
    for i in nodes do
        let proc = runtime.CreateProcess <@ getData i @>
        let (id,lst) = proc.AwaitResult()
        let tempS = ref 0
        for j in lst do
            let proc' = runtime.CreateProcess <@ getData j @>
            let (id',lst') = proc'.AwaitResult()
            tempS := !tempS + id'
        //printfn "%d" !tempS
        sums := List.append !sums [(id,!tempS)] 
    sums                                   

sum mutableNodes

///////////////////////////////AVERAGE//////////////////////////////////////
[<Cloud>]
let sendToNeighbor v (neighbors : List<IMutableCloudRef<Node<'T>>>) = 
        cloud {
            let i = (new System.Random()).Next(0, neighbors.Length-1)              
            let! N(id,lst) = MutableCloudRef.Read(neighbors.[i])                
            let! s = MutableCloudRef.Set(neighbors.[i],N(v,lst))        
            return i
        }

let average (nodes : List<IMutableCloudRef<Node<'T>>>) =         
    let safediv x y =
        match y with
            | 0 -> x
            | _ -> x/y
    //let av = ref List.empty
    for i in nodes do
        let proc = runtime.CreateProcess <@ getData i @>
        let (id,lst) = proc.AwaitResult()
        let isEmpty = 
            match lst with
                | [] -> true
                | _ -> false
        let tempS = ref 0
        if isEmpty then
            tempS := 0
        else             
            for j in lst do
                let proc' = runtime.CreateProcess <@ getData j @>
                let (id',lst') = proc'.AwaitResult()
                tempS := !tempS + id'
            //printfn "%d" !tempS
            //let x = runtime.Run <@ sendToNeighbor (safediv !tempS lst.Length) lst @>            //average
            let x = runtime.Run <@ sendToNeighbor !tempS lst @>         //sum   
            printfn "%d" x
            //av := List.append !av [(id,safediv !tempS lst.Length)] 
        //av  
    
//average mutableNodes
average mutableNodes
printGr mutableNodes