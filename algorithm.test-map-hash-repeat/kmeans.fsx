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
                        MutableCloudRef.New(N((1,(2.0,10.0),(Set.empty : (float*float) Set),Set.empty)))
                        MutableCloudRef.New(N((2,(2.0,5.0),(Set.empty : (float*float) Set),Set.empty)))
                        MutableCloudRef.New(N((3,(8.0,4.0),(Set.empty : (float*float) Set),Set.empty)))
                        MutableCloudRef.New(N((4,(5.0,8.0),(Set.empty : (float*float) Set),Set.empty)))
                        MutableCloudRef.New(N((5,(7.0,5.0),(Set.empty : (float*float) Set),Set.empty)))
                        MutableCloudRef.New(N((6,(6.0,4.0),(Set.empty : (float*float) Set),Set.empty)))
                        MutableCloudRef.New(N((7,(1.0,2.0),(Set.empty : (float*float) Set),Set.empty)))
                        MutableCloudRef.New(N((8,(4.0,9.0),(Set.empty : (float*float) Set),Set.empty)))

                    |]
                    |> Cloud.Parallel
    let! initMeans = 
        [|//for i in 1..k -> 
            //MutableCloudRef.New(N((1,[|rnd.Next(0,11) |> float;rnd.Next(0,11) |> float|],(Set.empty : (float*float) Set),Set.empty)))
            MutableCloudRef.New(N((-1,(2.0,10.0),(Set.empty : (float*float) Set),Set.empty)))
            MutableCloudRef.New(N((-2,(5.0,8.0),(Set.empty : (float*float) Set),Set.empty)))
            MutableCloudRef.New(N((-3,(1.0,2.0),(Set.empty : (float*float) Set),Set.empty)))
        |] 
        |> Cloud.Parallel
                                        
    return Array.append initVals initMeans
} 

//each node contains its neighbors' ids
[<Cloud>]
let createNeighbors (nodes : IMutableCloudRef<Node<'Id,'newV,'oldV>> []) = cloud   {    
    
    //find means
    let! getMeans = 
        [|for node in nodes -> cloud {
            let! cloudNode = MutableCloudRef.Read(node)
            match cloudNode with      
                | N(id,_,_,_) when id < 0->
                    return Some node
                | N(id,_,_,_) ->
                    return None
        }|]     
        |> Cloud.Parallel  
        
    
    let means = 
        getMeans 
        |> Array.choose (fun x -> match x with 
                                    |Some(ref) -> Some ref
                                    | None -> None) 
    let! getNodes = 
        [|for node in nodes -> cloud {
            let! cloudNode = MutableCloudRef.Read(node)
            match cloudNode with      
                | N(id,_,_,_) when id > 0->
                    return Some node
                | N(id,_,_,_) ->
                    return None
        }|]     
        |> Cloud.Parallel  
        
    
    let dataNodes = 
        getNodes 
        |> Array.choose (fun x -> match x with 
                                    |Some(ref) -> Some ref
                                    | None -> None)             
    //distance
    //let dist (arr1 : float[]) (arr2 : float[]) = 
    //    Array.fold2 (fun acc elem1 elem2 -> acc + pown (elem1 - elem2) 2) 0.0 arr1 arr2
    let dist (x1, y1) (x2, y2) : float =
        let xDistance = x1 - x2
        let yDistance = y1 - y2
        xDistance * xDistance + yDistance * yDistance
    
    //get the coordinates of the centers
    let! meanCoords = 
        [|for mean in means -> cloud {
                let! cloudNode = MutableCloudRef.Read(mean)
                match cloudNode with      
                    | N(id,coords,_,_) ->
                        return (id,coords)
        } |]
        |> Cloud.Parallel    

    //calculate distances between each node and each mean and return each (meanId,minDistance)    
    let! minPairs = 
        [| for node in dataNodes -> cloud {
            let! cloudNode = MutableCloudRef.Read(node)
            match cloudNode with      
                | N(_,coord,_,_) -> 
                    return (Array.map (fun (id,c) -> (id,dist c coord)) meanCoords) |> Array.minBy snd                     
        }
        |]                          
        |> Cloud.Parallel

    
    //create clusters
    let addN pairMap = cloud {  
        match pairMap with 
            | (nodeId,clusterId) ->
                for mean in means do
                    let! cloudNode = MutableCloudRef.Read(mean)
                    match cloudNode with                                                           
                        | N(id,newv,oldSet,newSet) when id = clusterId ->                                    
                            do! MutableCloudRef.Force(mean,N(id,newv,oldSet,newSet.Add(nodeId)))
                        | N(id,newv,oldSet,newSet) -> 
                            do! MutableCloudRef.Force(mean,N(id,newv,oldSet,newSet))                     
    }                   
    let! _ = 
        minPairs 
        |> Array.mapi (fun i (clusterId,minDist) -> (i+1,clusterId))    //node i will be added to cluster with id x 
        |> Array.toList                    
        |> seqMap addN 
    
    return (dataNodes,means)                                        
}


//compute new centers
[<Cloud>]
let compute (nodes : IMutableCloudRef<Node<'Id,'newV,'oldV>> []) (means : IMutableCloudRef<Node<'Id,'newV,'oldV>> [])  = cloud {
    
    let newVals (node : IMutableCloudRef<Node<'Id,'newV,'oldV>> ) = cloud {        
        //get the neighbors REFS of the given node                             
        let getNeighbors (node : IMutableCloudRef<Node<'Id,'newV,'oldV>> ) = cloud {
            let! cloudNode = MutableCloudRef.Read(node)
            match cloudNode with
                | N(_,_,_,setN) ->    
                    let neighborIds = Set.toList setN
                    return [for n in neighborIds -> nodes.[n-1]]                
        }                 
        let getCoords (node : IMutableCloudRef<Node<'Id,'newV,'oldV>> ) = cloud {
                let! cloudNode = MutableCloudRef.Read(node)
                match cloudNode with
                    | N(_,coords,_,_)  ->                 
                        return coords    
        }
        //get neighbors REFS from the given node
        let! neighborRefs = getNeighbors node   

        //concatenate given node's newv with the neighbors' newv and return a list with tuples: (nodeId,newvs)
        let! cloudNode = MutableCloudRef.Read(node)
        match cloudNode with   
            | N(id,_,_,_) ->
                let! clusterCoords = seqMap getCoords neighborRefs                 
                return (id, clusterCoords)  
    }
    //new means
    let! allCoords = 
        [|for mean in means -> newVals mean|] 
        |> Cloud.Parallel                      

    let xs = 
        Array.map (fun (id,coords) -> let xSum = List.fold (fun acc (x,y) -> acc + x) 0.0 coords
                                      xSum/(coords.Length |> float)) allCoords        
    let ys = 
        Array.map (fun (id,coords) -> let ySum =  List.fold (fun acc (x,y) -> acc + y) 0.0 coords 
                                      ySum/(coords.Length |> float)) allCoords            
    let newMeans = Array.mapi2 (fun i x y  -> (-(i+1),(x,y))) xs ys
    (*
    let changeMeans =         
        Array.mapi (fun i mean -> cloud {
            let! cloudNode = MutableCloudRef.Read(mean)
            match cloudNode with
                | N(id,coords,oldSet,newSet)  ->                 
                    do! MutableCloudRef.Force(mean,N(id,newMeans.[i],oldSet,newSet))
        }) means
    return! changeMeans |> Cloud.Parallel               *)
    return newMeans |> Map.ofArray
}


[<Cloud>]
let rec mapHashRepeat (nodes : IMutableCloudRef<Node<'Id,'newV,'oldV>> []) 
                        //(neighbors : Map<'nodeId,Set<'nodeId>>) 
                        compute isDone  = cloud {              
   
    //change old value with the new value
    let changeV (node : IMutableCloudRef<Node<'Id,'newV,'oldV>> ) comp = cloud {
        let! cloudNode = MutableCloudRef.Read(node)
        match cloudNode with        
            | N(id,currentV,oldv,setN)  ->                
                let newData = (id,comp,currentV,setN)                 
                do! MutableCloudRef.Force(node,N(newData))                
    }
    let! computations = compute nodes    
    let checkAll = 
        [|
            for node in nodes -> cloud {                   
                let! cloudNode = MutableCloudRef.Read(node)
                match cloudNode with   
                    | N(id,_,_,_) when Map.containsKey id computations ->
                        let comp = computations.[id]
                        let! ok = isDone node comp
                        //if not ok then
                            //finish := false
                        do! changeV node comp  
                        return ok  
            }
        |]
       
    let! check = Cloud.Parallel checkAll 
    let ok = check |> Seq.fold (fun acc item -> acc && item) true
    match ok with  
        | true -> return nodes          
        | false -> 
            return! mapHashRepeat nodes compute isDone //(ref true)                                
}
    


let runtime = MBrace.InitLocal 4
//number of data points, number of clusters
let allNodes = runtime.Run <@ createNodes 8 3 @>
let (nodes,means) = runtime.Run <@ createNeighbors allNodes @>
let c = runtime.Run <@ compute nodes means @>

[<Cloud>]
let isDone (node : IMutableCloudRef<Node<'Id,'newV,'oldV>> ) comp = cloud    {
    let! cloudNode = MutableCloudRef.Read(node)
    match cloudNode with 
        | N(id, currentV, oldV,_) -> return currentV = comp   
}

let result = runtime.Run <@ mapHashRepeat nodes compute isDone  @>                                                                     


//let result = runtime.Run <@ mapHashRepeat nodes (fun x -> x) dist isDone @>                                                                     

[<Cloud>]
let printCloudNodes (nodes : IMutableCloudRef<Node<'Id,'newV,'oldV>> [])= cloud {
    return! seqMap (fun node -> MutableCloudRef.Read(node)) (nodes |> Array.toList)
}

runtime.Run <@ printCloudNodes allNodes @>
runtime.Run <@ printCloudNodes means @>
//runtime.Run <@ printCloudNodes newN @>
