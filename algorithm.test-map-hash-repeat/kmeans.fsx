//http://webdocs.cs.ualberta.ca/~zaiane/courses/cmput695/F07/exercises/Exercises695Clus-solution.pdf

#r "Nessos.MBrace.Utils"
#r "Nessos.MBrace.Actors"
#r "Nessos.MBrace.Base"
#r "Nessos.MBrace.Store"
#r "Nessos.MBrace.Client"

#r "../lib/bin/Debug/Nessos.MBrace.Lib.dll"

open Nessos.MBrace.Client

//id,new value,old value, set<ids>
type Node<'Id,'newV,'oldV 
            when 'Id : comparison and 'oldV : comparison> = 
                                        | N of 'Id*'newV*'oldV* Set<'Id>

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


//example-1    
[<Cloud>]
let createNodes1 (numData : int) (k : int) = cloud {    
    let rnd = System.Random() 
    let! initVals = [| 
                        MutableCloudRef.New(N((1,(1.0,1.0),(Set.empty : int Set) ,Set.empty)))
                        MutableCloudRef.New(N((2,(1.5,2.0),(Set.empty : int Set) ,Set.empty)))
                        MutableCloudRef.New(N((3,(3.0,4.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((4,(5.0,7.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((5,(3.5,5.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((6,(4.5,5.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((7,(3.5,4.5),(Set.empty : int Set),Set.empty)))
                        
                    |]
                    |> Cloud.Parallel
    let! initCenters = 
        [|//for i in 1..k -> 
            //MutableCloudRef.New(N((1,[|rnd.Next(0,11) |> float;rnd.Next(0,11) |> float|],(Set.empty : (float*float) Set),Set.empty)))
            MutableCloudRef.New(N((-1,(1.0,1.0),(Set.empty : int Set),Set.empty)))
            MutableCloudRef.New(N((-2,(5.0,7.0),(Set.empty : int Set),Set.empty)))
        |] 
        |> Cloud.Parallel
                                        
    return Array.append initVals initCenters
} 


//creates num Nodes with random new value (1..10)                   
//example-2
[<Cloud>]
let createNodes2 (numData : int) (k : int) = cloud {    
    let rnd = System.Random() 
    let! initVals = [| 
                        MutableCloudRef.New(N((1,(2.0,10.0),(Set.empty : int Set) ,Set.empty)))
                        MutableCloudRef.New(N((2,(2.0,5.0),(Set.empty : int Set) ,Set.empty)))
                        MutableCloudRef.New(N((3,(8.0,4.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((4,(5.0,8.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((5,(7.0,5.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((6,(6.0,4.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((7,(1.0,2.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((8,(4.0,9.0),(Set.empty : int Set),Set.empty)))
                        
                    |]
                    |> Cloud.Parallel
    let! initCenters = 
        [|//for i in 1..k -> 
            //MutableCloudRef.New(N((1,[|rnd.Next(0,11) |> float;rnd.Next(0,11) |> float|],(Set.empty : (float*float) Set),Set.empty)))
            MutableCloudRef.New(N((-1,(2.0,10.0),(Set.empty : int Set),Set.empty)))
            MutableCloudRef.New(N((-2,(5.0,8.0),(Set.empty : int Set),Set.empty)))
            MutableCloudRef.New(N((-3,(1.0,2.0),(Set.empty : int Set),Set.empty)))
        |] 
        |> Cloud.Parallel
                                        
    return Array.append initVals initCenters
} 

//example-3
[<Cloud>]
let createNodes3 (numData : int) (k : int) = cloud {    
    let rnd = System.Random() 
    let! initVals = [| 
                        MutableCloudRef.New(N((1,(1.0,1.0),(Set.empty : int Set) ,Set.empty)))
                        MutableCloudRef.New(N((2,(2.0,1.0),(Set.empty : int Set) ,Set.empty)))
                        MutableCloudRef.New(N((3,(4.0,3.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((4,(5.0,4.0),(Set.empty : int Set),Set.empty)))
                    |]
                    |> Cloud.Parallel
    let! initCenters = 
        [|//for i in 1..k -> 
            //MutableCloudRef.New(N((1,[|rnd.Next(0,11) |> float;rnd.Next(0,11) |> float|],(Set.empty : (float*float) Set),Set.empty)))
            MutableCloudRef.New(N((-1,(1.0,1.0),(Set.empty : int Set),Set.empty)))
            MutableCloudRef.New(N((-2,(2.0,1.0),(Set.empty : int Set),Set.empty)))
        |] 
        |> Cloud.Parallel
                                        
    return Array.append initVals initCenters
} 

//example-4
[<Cloud>]
let createNodes4 (numData : int) (k : int) = cloud {    
    let rnd = System.Random() 
    let! initVals = [| 
                        MutableCloudRef.New(N((1,(1.0,3.0),(Set.empty : int Set) ,Set.empty)))
                        MutableCloudRef.New(N((2,(3.0,3.0),(Set.empty : int Set) ,Set.empty)))
                        MutableCloudRef.New(N((3,(5.0,3.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((4,(7.0,3.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((5,(9.0,3.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((6,(11.0,3.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((7,(13.0,3.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((8,(15.0,3.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((9,(17.0,3.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((10,(1.0,0.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((11,(3.0,0.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((12,(5.0,0.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((13,(7.0,0.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((14,(9.0,0.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((15,(11.0,0.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((16,(13.0,0.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((17,(15.0,0.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((18,(17.0,0.0),(Set.empty : int Set),Set.empty)))
                    |]
                    |> Cloud.Parallel
    let! initCenters = 
        [|//for i in 1..k -> 
            //MutableCloudRef.New(N((1,[|rnd.Next(0,11) |> float;rnd.Next(0,11) |> float|],(Set.empty : (float*float) Set),Set.empty)))
            MutableCloudRef.New(N((-1,(9.0,3.0),(Set.empty : int Set),Set.empty)))
            MutableCloudRef.New(N((-2,(11.0,3.0),(Set.empty : int Set),Set.empty)))
        |] 
        |> Cloud.Parallel
                                        
    return Array.append initVals initCenters
} 

//example-5
[<Cloud>]
let createNodes5 (numData : int) (k : int) = cloud {    
    let rnd = System.Random() 
    let! initVals = [| 
                        MutableCloudRef.New(N((1,(2.0,10.0),(Set.empty : int Set) ,Set.empty)))
                        MutableCloudRef.New(N((2,(2.0,5.0),(Set.empty : int Set) ,Set.empty)))
                        MutableCloudRef.New(N((3,(8.0,4.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((4,(5.0,8.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((5,(7.0,5.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((6,(6.0,4.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((7,(1.0,2.0),(Set.empty : int Set),Set.empty)))
                        MutableCloudRef.New(N((8,(4.0,9.0),(Set.empty : int Set),Set.empty)))
                    |]
                    |> Cloud.Parallel
    let! initCenters = 
        [|//for i in 1..k -> 
            //MutableCloudRef.New(N((1,[|rnd.Next(0,11) |> float;rnd.Next(0,11) |> float|],(Set.empty : (float*float) Set),Set.empty)))
            MutableCloudRef.New(N((-1,(2.0,10.0),(Set.empty : int Set),Set.empty)))
            MutableCloudRef.New(N((-2,(5.0,8.0),(Set.empty : int Set),Set.empty)))
            MutableCloudRef.New(N((-3,(1.0,2.0),(Set.empty : int Set),Set.empty)))
        |] 
        |> Cloud.Parallel
                                        
    return Array.append initVals initCenters
} 



//each node contains its neighbors' ids
[<Cloud>]
let createNeighbors (nodes : IMutableCloudRef<Node<'Id,'newV,'oldV>> []) = cloud   {    
    
    //find the centers (nodes with id = -1)
    let! getCenters = 
        [|for node in nodes -> cloud {
            let! cloudNode = MutableCloudRef.Read(node)
            match cloudNode with      
                | N(id,_,_,_) when id < 0->
                    return Some node
                | N(id,_,_,_) ->
                    return None
        }|]     
        |> Cloud.Parallel  
        
    //centers
    let centers = 
        getCenters 
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
    let! centerCoords = 
        [|for center in centers -> cloud {
                let! cloudNode = MutableCloudRef.Read(center)
                match cloudNode with      
                    | N(id,coords,_,_) ->
                        return (id,coords)
        } |]
        |> Cloud.Parallel    

    //calculate distances between each node and each center and return (centerId,minDistance) pairs
    let! minPairs = 
        [| for node in dataNodes -> cloud {
            let! cloudNode = MutableCloudRef.Read(node)
            match cloudNode with      
                | N(_,coord,_,_) -> 
                    return (Array.map (fun (id,c) -> (id,dist c coord)) centerCoords) |> Array.minBy snd                     
        }
        |]                          
        |> Cloud.Parallel

    
    //create clusters
    let addN pairMap = cloud {  
        match pairMap with 
            | (nodeId,clusterId) ->
                for center in centers do
                    let! cloudNode = MutableCloudRef.Read(center)
                    match cloudNode with                                                           
                        | N(id,newv,oldSet,newSet) when id = clusterId ->                                    
                            do! MutableCloudRef.Force(center,N(id,newv,oldSet,newSet.Add(nodeId)))
                        | N(id,newv,oldSet,newSet) -> 
                            do! MutableCloudRef.Force(center,N(id,newv,oldSet,newSet))                     
    }                   
    let! _ = 
        minPairs 
        |> Array.mapi (fun i (clusterId,minDist) -> (i+1,clusterId))    //node i will be added to cluster with id x 
        |> Array.toList                    
        |> seqMap addN 
    
    return (dataNodes,centers)                                        
}


//compute the new centers
[<Cloud>]
let compute (nodes : IMutableCloudRef<Node<'Id,'newV,'oldV>> []) 
            (centers : IMutableCloudRef<Node<'Id,'newV,'oldV>> [])  = cloud {
    
    let newVals (center : IMutableCloudRef<Node<'Id,'newV,'oldV>> ) = cloud {        
        //get the refs of the nodes who belong to the center
        let getNeighbors (center : IMutableCloudRef<Node<'Id,'newV,'oldV>> ) = cloud {
            let! cloudNode = MutableCloudRef.Read(center)
            match cloudNode with
                | N(_,_,_,setN) ->    
                    let neighborIds = Set.toList setN
                    return [for n in neighborIds -> nodes.[n-1]]   //node.[0] has id 1 etc ...             
        }                 
        //get the coordinates (2nd element) of the given node
        let getCoords (node : IMutableCloudRef<Node<'Id,'newV,'oldV>> ) = cloud {
                let! cloudNode = MutableCloudRef.Read(node)
                match cloudNode with
                    | N(_,coords,_,_)  ->                 
                        return coords    
        }
        //get neighbors REFS from the given center
        let! neighborRefs = getNeighbors center   

        //concatenate given center's coordinates with the neighbors' coordinates and return a list with tuples: (nodeId,coordinates)
        let! cloudNode = MutableCloudRef.Read(center)
        match cloudNode with   
            | N(id,_,_,_) ->
                let! clusterCoords = seqMap getCoords neighborRefs                 
                return (id, clusterCoords)  
    }

    //for each cluster, returns a list with the coordinates which will calculate the new center
    let! allCoords = 
        [|for center in centers -> newVals center|] 
        |> Cloud.Parallel                      
    
    //calculates the x coordinate for each new center
    let xs = 
        Array.map (fun (id,coords) -> let xSum = List.fold (fun acc (x,y) -> acc + x) 0.0 coords
                                      xSum/(coords.Length |> float)) allCoords        
    //calculates the y coordinate for each new center
    let ys = 
        Array.map (fun (id,coords) -> let ySum =  List.fold (fun acc (x,y) -> acc + y) 0.0 coords 
                                      ySum/(coords.Length |> float)) allCoords            
    
    
    let newCenters = 
        Array.mapi2 (fun i x y  -> (-(i+1),(x,y))) xs ys
        |> Map.ofArray
    //return newCenters.[-3]
    //(*
    let changeCenters =         
        Array.map (fun center -> cloud {
            let! cloudNode = MutableCloudRef.Read(center)
            match cloudNode with
                | N(id,coords,oldSet,newSet)  ->                 
                    do! MutableCloudRef.Force(center,N(id,newCenters.[id],oldSet,newSet))
        }) centers
    return! changeCenters |> Cloud.Parallel               
}

[<Cloud>]
let computeNeighbors (dataNodes : IMutableCloudRef<Node<'Id,'newV,'oldV>> []) 
                      (centers : IMutableCloudRef<Node<'Id,'newV,'oldV>> [])    = cloud {

    let dist (x1, y1) (x2, y2) : float =
            let xDistance = x1 - x2
            let yDistance = y1 - y2
            xDistance * xDistance + yDistance * yDistance
    
    //get the coordinates of the centers
    let! centerCoords = 
        [|for center in centers -> cloud {
                let! cloudNode = MutableCloudRef.Read(center)
                match cloudNode with      
                    | N(id,coords,_,_) ->
                        return (id,coords)
        } |]
        |> Cloud.Parallel    

    //calculate distances between each node and each center and returns (centerId,minDistance) pairs
    //first pair is for node with id = 1, second pair for node with id = 2 etc.
    let! minPairs = 
        [| for node in dataNodes -> cloud {
            let! cloudNode = MutableCloudRef.Read(node)
            match cloudNode with      
                | N(_,coord,_,_) -> 
                    return (Array.map (fun (id,c) -> (id,dist c coord)) centerCoords) |> Array.minBy snd                     
        }
        |]                          
        |> Cloud.Parallel
  
    //returns a list with (cId,nId) which means that 
    //node with id nId belongs to cluster with id cId
    return 
        minPairs
        |> Array.mapi (fun i (cId,minDist) -> (i+1,cId) )
        |> Seq.groupBy snd |> Seq.map (fun (cId,seqnIds) -> (cId, (Seq.map (fun (nid,cid) -> nid) seqnIds) |> Set.ofSeq ))
        |> Seq.toArray 
        |> Map.ofArray
}


[<Cloud>]
let rec mapHashRepeat (dataNodes : IMutableCloudRef<Node<'Id,'newV,'oldV>> []) 
                      (centers : IMutableCloudRef<Node<'Id,'newV,'oldV>> []) 
                      calcCenters  
                      computeNeighbors
                      isDone  = cloud {              
   
    //change old coords with the new coords
    
    let changeV (node : IMutableCloudRef<Node<'Id,'newV,'oldV>> ) comp = cloud {
        let! cloudNode = MutableCloudRef.Read(node)
        match cloudNode with        
            | N(id,coords,oldSet,currentV)  ->                
                let newData = (id,coords,currentV,comp)                 
                do! MutableCloudRef.Force(node,N(newData))                
    }    
    
    let! computations = calcCenters dataNodes centers 
    let! neighborPairs = computeNeighbors dataNodes centers
    //do! changeV centers.[0] computations.[-2]
    
    
    let checkAll = 
        [|
            for node in centers -> cloud {                   
                let! cloudNode = MutableCloudRef.Read(node)
                match cloudNode with   
                    | N(id,_,_,_) when Map.containsKey (id) neighborPairs ->
                        let comp = neighborPairs.[id]
                        let! ok = isDone node comp
                        do! changeV node comp  
                        return ok  
            }
        |]
       
    let! check = Cloud.Parallel checkAll 
    let ok = check |> Seq.fold (fun acc item -> acc && item) true
    match ok with  
        | true -> return (dataNodes,centers)
        | false -> 
            return! mapHashRepeat dataNodes centers calcCenters computeNeighbors isDone //(ref true)  
    
}
    
    

let runtime = MBrace.InitLocal 4
//number of data points, number of clusters, currently not used
let allNodes = runtime.Run <@ createNodes1 7 2 @>
let (nodes,centers) = runtime.Run <@ createNeighbors allNodes @>

//let c = runtime.Run <@ compute nodes centers @>

[<Cloud>]
let isDone (node : IMutableCloudRef<Node<'Id,'newV,'oldV>> ) comp = cloud    {
    let! cloudNode = MutableCloudRef.Read(node)
    match cloudNode with 
        | N(id, coords, setN,currentSet) -> return currentSet = comp   
}

let (finalNodes,finalCenters) = runtime.Run <@ mapHashRepeat nodes centers compute computeNeighbors isDone  @>                                                                     

[<Cloud>]
let printCloudNodes (nodes : IMutableCloudRef<Node<'Id,'newV,'oldV>> [])= cloud {
    return! seqMap (fun node -> MutableCloudRef.Read(node)) (nodes |> Array.toList)
}

runtime.Run <@ printCloudNodes nodes @>
runtime.Run <@ printCloudNodes centers @>
runtime.Run <@ printCloudNodes finalNodes @>
runtime.Run <@ printCloudNodes finalCenters @>