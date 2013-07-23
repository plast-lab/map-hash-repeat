
// BEGIN PREAMBLE -- do not evaluate, for intellisense only
#r "Nessos.MBrace.Utils"
#r "Nessos.MBrace.Actors"
#r "Nessos.MBrace.Base"
#r "Nessos.MBrace.Store"
#r "Nessos.MBrace.Client"

#r "../lib/bin/Debug/Nessos.MBrace.Lib.dll"

open Nessos.MBrace.Client

type Node<'T> =  | N of 'T * List<ICloudRef<Node<'T>>>  
type Graph<'T>= | G of List<ICloudRef<Node<'T>>>

[<Cloud>]
let cloudNode node = 
    cloud {     
        return! newRef <| N(node)
    }

//////////////////////////////////////////////////////////
[<Cloud>]
let demo (number: int) : ICloud<int> =
    cloud {
        let! num =  
            cloud {               
               return 1+1 } 
            <||>
            cloud {            
                Cloud.OfAsync <| Async.Sleep 5000  
                return 1}
       
        return! cloud { return fst num + number + 1 }
    }           
//////////////////////////////////////////////////////////

[<Cloud>]
let addNeighbor (node1 : ICloudRef<Node<'T>>) (node2 : ICloudRef<Node<'T>>)  =
    cloud {
        match node1.Value with
            | N(id,[]) -> return! newRef <| N(id,[node2])
            | N (id,nList) -> return! newRef <| N(id,List.append nList [node2]) 
    }           
    
[<Cloud>]
let createCloudGraph2 (nodes : List<ICloudRef<Node<'T>>>) = 
    cloud {
        return! newRef <| G(nodes)
    }

let n1 = (1,[])
let n2 = (2,[])
let n3 = (3,[])
let n4 = (4,[])
let n5 = (5,[])

// create a local-only runtime
let runtime = MBrace.InitLocal 4

runtime.Run <@ demo 5 @>


let n1' = runtime.CreateProcess <@ cloudNode n1 @>
let n2' = runtime.CreateProcess <@ cloudNode n2 @>
let n3' = runtime.CreateProcess <@ cloudNode n3 @>
let n4' = runtime.CreateProcess <@ cloudNode n4 @>
let n5' = runtime.CreateProcess <@ cloudNode n5 @>

let n1Ref = n1'.AwaitResult()
let n2Ref = n2'.AwaitResult()
let n3Ref = n3'.AwaitResult()
let n4Ref = n4'.AwaitResult()
let n5Ref = n5'.AwaitResult()

//5->4
let n5_4 = runtime.CreateProcess <@ addNeighbor n5Ref n4Ref @>
let n5Ref' = n5_4.AwaitResult()

//3->4
let n3_4 = runtime.CreateProcess <@ addNeighbor n3Ref n4Ref @>
let n3Ref' = n3_4.AwaitResult()

//2->3
let n2_3 = runtime.CreateProcess <@ addNeighbor n2Ref n3Ref' @>
let n2Ref' = n2_3.AwaitResult()

//1->3
let n1_3 = runtime.CreateProcess <@ addNeighbor n1Ref n3Ref' @>
let n1Ref' = n1_3.AwaitResult()

//1->5
let n1_5 = runtime.CreateProcess <@ addNeighbor n1Ref' n5Ref' @>
let n1Ref'' = n1_5.AwaitResult()


let g = runtime.CreateProcess <@ createCloudGraph2 [n1Ref'';n2Ref';n3Ref';n4Ref;n5Ref'] @>
let result = g.AwaitResult()        //a graph

//print each node and its neighbors
let printNeighbors (graph : ICloudRef<Graph<'T>>) = 
    match graph.Value with
            | G(nList) -> 
            nList |> List.iter (fun x -> 
                match x.Value with                     
                    | N(id,nlist) ->                   
                        printfn "Node %d: " id      
                        nlist |> List.iter (fun n ->
                            match n.Value with                            
                            | N (id,_) -> printfn "%d," id) ) 

printNeighbors result

//sum all node ids of the given list
let rec sum (nlist : List<ICloudRef<Node<'T>>>) =
    match nlist with
        | head :: tail -> 
            match head.Value with 
                | N(id,_) -> id + sum tail
        | [] -> 0


//for each node in graph, print the node id and the sum of his neighbors' ids
let sumN (graph : ICloudRef<Graph<'T>>) (sum : List<'I> -> int) = 
    let sums = List.empty
    match graph.Value with
            | G(nList) -> 
            nList |> List.iter (fun x -> 
                match x.Value with                     
                    | N(id,nlist) ->                   
                        printf "Node %d: " id    
                        printfn "%d" (sum nlist)
                        ) 
                        

sumN result sum


//average of all node ids of the given list
let rec average (nlist : List<ICloudRef<Node<'T>>>) =
    match nlist with
    | head :: tail -> 
        match head.Value with 
            | N(id,_) -> 
            let s = id + sum tail
            s / nlist.Length
    | [] -> 0



//for each node in graph, print the node id and the average of his neighbors' ids
let averageN (graph : ICloudRef<Graph<'T>>) (average : List<'I> -> int) = 
    let sums = List.empty
    match graph.Value with
            | G(nList) -> 
            nList |> List.iter (fun x -> 
                match x.Value with                     
                    | N(id,nlist) ->                   
                        printf "Node %d: " id    
                        printfn "%d" (average nlist)
                        ) 

averageN result average
