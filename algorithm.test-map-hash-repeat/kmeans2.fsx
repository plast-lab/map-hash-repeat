//ta kentra einai mesa stous nodes

//map-rehash-repeat "average computation" in F# - {m}brace
#r "Nessos.MBrace.Utils"
#r "Nessos.MBrace.Actors"
#r "Nessos.MBrace.Base"
#r "Nessos.MBrace.Store"
#r "Nessos.MBrace.Client"

#r "../lib/bin/Debug/Nessos.MBrace.Lib.dll"

open Nessos.MBrace.Client

//id,new value,old value, set<ids>
type Node<'Id,'newV,'oldV when 'Id : comparison and 'oldV : comparison> = | N of 'Id*'newV*'oldV* Set<'Id>

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
    
//creates num Nodes with random new value (1..10)                   
[<Cloud>]
let createNodes (numData : int) (k : int) = cloud {    
    let rnd = System.Random() 
    let! initVals = [| 
                        MutableCloudRef.New(N((1,[|2.0;10.0|],(Set.empty : (float*float) Set),Set.empty)))
                        MutableCloudRef.New(N((2,[|2.0;5.0|],(Set.empty : (float*float) Set),Set.empty)))
                        MutableCloudRef.New(N((3,[|8.0;4.0|],(Set.empty : (float*float) Set),Set.empty)))
                        MutableCloudRef.New(N((4,[|5.0;8.0|],(Set.empty : (float*float) Set),Set.empty)))
                        MutableCloudRef.New(N((5,[|7.0;5.0|],(Set.empty : (float*float) Set),Set.empty)))
                        MutableCloudRef.New(N((6,[|6.0;4.0|],(Set.empty : (float*float) Set),Set.empty)))
                        MutableCloudRef.New(N((7,[|1.0;2.0|],(Set.empty : (float*float) Set),Set.empty)))
                        MutableCloudRef.New(N((8,[|4.0;9.0|],(Set.empty : (float*float) Set),Set.empty)))

                    |]
                    |> Cloud.Parallel                                            
    return initVals
} 

//each node contains its neighbors' ids
[<Cloud>]
let createNeighbors (nodes : IMutableCloudRef<Node<'Id,'newV,'oldV>> []) (centers : int []) = cloud   {    
    
    //distance
    let dist (arr1 : float[]) (arr2 : float[]) = 
        Array.fold2 (fun acc elem1 elem2 -> acc + pown (elem1 - elem2) 2) 0.0 arr1 arr2
    
    //get the coordinates of the centers
    let! coords = 
        [|for c in centers -> cloud {
                let! cloudNode = MutableCloudRef.Read(nodes.[c-1])
                match cloudNode with      
                    | N(id,coords,_,_) ->
                        return (id,coords)
        } |]
        |> Cloud.Parallel    

    //calculate distances between each node and each center
    let! minPairs = 
        [| for node in nodes -> cloud {
            let! cloudNode = MutableCloudRef.Read(node)
            match cloudNode with      
                | N(_,coord,_,_) -> 
                    return (Array.map (fun (id,c) -> (id,dist c coord)) coords) |> Array.minBy snd                     
        }
        |]                          
        |> Cloud.Parallel
    //let xx = minPairs |> Array.map (fun (x,y) -> x)     
    let xx = minPairs |> Array.mapi (fun i (x,y) -> (i,x))     
    let addN x = cloud {  
        match x with 
            |(nodeId, centerId) ->                                
            for nd in nodes do
                let! cloudNode = MutableCloudRef.Read(nd)
                match cloudNode with                                                           
                    | N(id,newv,oldSet,newSet) when id = centerId ->                                    
                        do! MutableCloudRef.Force(nd,N(id,newv,oldSet,newSet.Add(nodeId+1)))
                    | N(id,newv,oldSet,newSet) -> 
                        do! MutableCloudRef.Force(nd,N(id,newv,oldSet,newSet))                     
    }     
    return! seqMap addN (xx |> Array.toList)
    (*
    let addN i x = cloud {  
        let! n = MutableCloudRef.Read(nodes.[i])
        let nodeId = 
            match n with
                | N(nId,_,_,_) -> nId 
        for nd in nodes do
            let! cloudNode = MutableCloudRef.Read(nd)
            match cloudNode with                                                           
                | N(id,newv,oldSet,newSet) when id = x ->                                    
                    do! MutableCloudRef.SpinSet(nd,(fun n ->N(id,newv,oldSet,newSet.Add(nodeId))))
                | N(id,newv,oldSet,newSet) -> 
                    do! MutableCloudRef.SpinSet(nd,(fun n ->N(id,newv,oldSet,newSet)))                         
    }     
    let y =  Array.mapi addN xx                                  
    return! y |> Cloud.Parallel
    *)
}

let runtime = MBrace.InitLocal 4
//number of data points, number of clusters
let nodes = runtime.Run <@ createNodes 8 3 @>
runtime.Run <@ createNeighbors nodes [|1;4;7|] @>


[<Cloud>]
let isDone (node : IMutableCloudRef<Node<'Id,'newV,'oldV>> ) comp = cloud    {
    let! cloudNode = MutableCloudRef.Read(node)
    match cloudNode with 
        | N(id, currentV, oldV,_) -> return currentV = comp   
}
//let result = runtime.Run <@ mapHashRepeat nodes (fun x -> x) dist isDone @>                                                                     

[<Cloud>]
let printCloudNodes (nodes : IMutableCloudRef<Node<'Id,'newV,'oldV>> [])= cloud {
    return! seqMap (fun node -> MutableCloudRef.Read(node)) (nodes |> Array.toList)
}

runtime.Run <@ printCloudNodes nodes @>
//runtime.Run <@ printCloudNodes newN @>


