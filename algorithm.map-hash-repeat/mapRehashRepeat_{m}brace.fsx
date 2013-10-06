//map-rehash-repeat "average computation" in F# - {m}brace

#r "Nessos.MBrace.Utils"
#r "Nessos.MBrace.Actors"
#r "Nessos.MBrace.Base"
#r "Nessos.MBrace.Store"
#r "Nessos.MBrace.Client"

#r "../lib/bin/Debug/Nessos.MBrace.Lib.dll"

open Nessos.MBrace.Client

//id,new value, old value
type Node<'Id,'newV,'oldV> = | N of 'Id*'newV*'oldV

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
let createNodes (num : int)= cloud {    
    let rnd = System.Random()  
    return! [| for n in 0 .. num -> (n,rnd.Next (1,11),0) |] |> Array.toList |> seqMap (fun node -> MutableCloudRef.New(N(node)))        
}        

let createNeighbors (nodes : IMutableCloudRef<Node<'Id,'newV,'oldV>> list) (nList : List<int*int>) = 
    [| for n in nList ->
        match n with
            | (parent,neighbor) ->
                (parent,neighbor)
    |] 
    |> Seq.groupBy fst |> Seq.map (fun (key,values) -> (key,values |> Seq.map (fun (k1,v1) -> v1) |> Set.ofSeq)) 
    |> Seq.toArray 
    |> Map.ofArray



//average
let compute (vals : 'a list) = 
    List.sum vals / vals.Length


[<Cloud>]
let rec mapRehashRepeat (nodes : IMutableCloudRef<Node<'Id,'newV,'oldV>> list) 
                    (neighbors : Map<'id,Set<'id>>) 
                    compute isDone finish = cloud {

    //get the neighbors (refs) of the given node                             
    let receive (node : IMutableCloudRef<Node<'Id,'newV,'oldV>> ) = cloud {
        let! cloudNode = MutableCloudRef.Read(node)
        match cloudNode with
            | N(id,_,_) ->
                let myNeighborsSet = Map.filter (fun key value -> key = id) neighbors 
                                    |> Map.toArray 
                                    |> Array.map (fun (key,value) -> value) 
                return [| for n in myNeighborsSet.[0] -> nodes.[n] |]                   
    }    
    //return! receive nodes.[0]
    
    //sets the new value of the given node changes to the average. old value is the previous new value
    let changeV (node : IMutableCloudRef<Node<'Id,'newV,'oldV>> ) comp = cloud {
        let! cloudNode = MutableCloudRef.Read(node)
        match cloudNode with        
            |N(id,currentV,oldv)  ->                
                let newData = (id,comp,currentV)                 
                do! MutableCloudRef.Force(node,N(newData))                
    }
                           
    return () 
}
    

let finish = ref true
let runtime = MBrace.InitLocal 4
let nodes = runtime.Run <@ createNodes 6 @>
let neighbors = createNeighbors nodes [(0,1);(1,0);(1,2);(2,1);(1,3);(3,1);(2,4);(4,2);(3,4);(4,3);(4,5);(5,4)]
let nn = runtime.Run <@ mapRehashRepeat nodes neighbors compute (fun () -> ()) finish @>




[<Cloud>]
let printCloudNodes (nodes : IMutableCloudRef<Node<'Id,'newV,'oldV>> list)= cloud {
    return! seqMap (fun node -> MutableCloudRef.Read(node)) nodes
}

runtime.Run <@ printCloudNodes nodes @>
(*
//id,new value, old value
type Node<'Id,'newV,'oldV> = | N of IMutableCloudRef<('Id*'newV*'oldV)>

///
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
///


//creates num Nodes with random new value (1..10)                   
[<Cloud>]
let createNodes (num : int)= cloud {    
    let rnd = System.Random()  
    let! mr = MutableCloudRef.New((1,2,3))
    //return N(mr)
    let vals = [| for n in 0 .. num -> MutableCloudRef.New((n,rnd.Next (1,11),0)) |] 
    let! initVals = Cloud.Parallel vals
    return [ for d in initVals -> N(d) ]
}        

//array apo tuple<node,array me neighbors>
let createNeighbors (nodes : 'refs list) (nList : List<int*int>) = 
    [| for n in nList ->
        match n with
            | (fromN,toN) ->
                (nodes.[fromN],nodes.[toN])
    |] 
    |> Seq.groupBy fst |> Seq.map (fun (key,values) -> (key,values |> Seq.map (fun (k1,v1) -> v1) |> Seq.toArray)) 
    |> Seq.toArray 

//array apo tuple<nodeId,set of neighborsIds>
let createNeighbors1 (nodes : 'refs list) (nList : List<int*int>) = 
    [| for n in nList ->
        match n with
            | (fromN,toN) ->
                (fromN,toN)
    |] 
    |> Seq.groupBy fst |> Seq.map (fun (key,values) -> (key,values |> Seq.map (fun (k1,v1) -> v1) |> Set.ofSeq)) 
    |> Seq.toArray 
    |> Map.ofArray
    

// create a local-only runtime
let runtime = MBrace.InitLocal 4
let nodes = runtime.Run<@ createNodes 6 @>
let neighbors = createNeighbors1 nodes [(0,1);(1,0);(1,2);(2,1);(1,3);(3,1);(2,4);(4,2);(3,4);(4,3);(4,5);(5,4)]





////
[<Cloud>]
let printN nodes  = 
    let getNode (node : Node<int,int,int>) = 
        cloud {
            match node with
                | N(data) ->
                    return! MutableCloudRef.Read(data)        
        } 
    cloud { return! seqMap getNode nodes }

runtime.Run <@printN nodes @>
///
*)

//2nd attemp
(*
type Node<'Id,'newV,'oldV> = | N of IMutableCloudRef<'Id*'newV*'oldV> 

///
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


[<Cloud>]
let createNodes (num : int)= cloud {    
    let rnd = System.Random()     
    return! seqMap (fun value -> 
                        cloud { 
                            let! mr = MutableCloudRef.New(value) 
                            return N(mr)
                        } 
                    ) [for n in 1 .. num -> (n,rnd.Next(1,11),0)]                 
}       
 
let createNeighbors (nodes : 'mRefs []) (nList : List<int*int>) = 
    [| for n in nList ->
        match n with
            | (fromN,toN) ->
                (nodes.[fromN],nodes.[toN])
    |] 
    |> Seq.groupBy fst |> Seq.map (fun (key,values) -> (key,values |> Seq.map (fun (k1,v1) -> v1) |> Seq.toArray)) 
    |> Seq.toArray 

    
//average
let compute (vals : 'a list) = 
    List.sum vals / vals.Length

[<Cloud>]
let rec mapRehashRepeat (nodes : 'mRefs []) 
                    (neighbors : ('mRef * 'mRef [] )[]) 
                    compute  = cloud  {

    //get the neighbors of the given node                             
    let receive (node : 'mRef) = 
        Array.filter (fun (parent,rest) -> parent = node) neighbors |> Array.map snd                                               

    //sets the new value of the given node changes to the average. old value is the previous new value
    let changeV (node : 'mRef) comp = cloud {   
        let! mrNode = MutableCloudRef.Read(node)
        match mrNode with
            | N(data) -> 
                let (id,currentV,oldv) = data
                MutableCloudRef.Force(mr,(id,comp,currentV))
    }
    
    changeV nodes.[0] 1000            
}

   (* 
    //calculates the average values for each node and returns a Map<node,av>
    let newVals = 
        [| for node in nodes ->
            let myNewVal node = 
                match node with
                    |N(data) -> 
                    let (_,newv,_) = !data
                    newv
            let vals = [myNewVal node] @ (receive node 
                        |> Set.toList 
                        |> List.map (fun elem -> 
                                        match elem with 
                                            | N(data) -> 
                                                let (_,newv,_) = !data 
                                                newv))                    
            let av = compute vals                        
            (node,av)            
        |]
        |> Map.ofArray        
    for node in nodes do
        let comp = newVals.[node]        
        if not (isDone node comp) then
            finish :=  false                        
        changeV node comp                                         
    match !finish with  
        | true -> nodes          
        | false -> 
            mapRehashRepeat nodes neighbors compute isDone (ref true)
            *)

let finish = ref true     
let runtime = MBrace.InitLocal 4
let nodes = runtime.Run <@ createNodes 6 @>
let neighbors =  createNeighbors nodes [(0,1);(1,0);(1,2);(2,1);(1,3);(3,1);(2,4);(4,2);(3,4);(4,3);(4,5);(5,4)]
mapRehashRepeat nodes neighbors compute 





type Node<'I> = | NN of IMutableCloudRef<'I> 
 

[<Cloud>]
let rec simpleNodes () = 
    cloud { 
        return! seqMap (fun value -> 
                            cloud { 
                                let! mr = MutableCloudRef.New(value) 
                                return NN(mr)
                            } 
                        ) [1 .. 5]         
    }


let node = runtime.Run <@ simpleNodes () @>

[<Cloud>]
let readNode nodes = cloud {
    return! seqMap (fun node -> cloud {
                        match node with
                            | N(data) ->
                                let! mr = MutableCloudRef.Read(data)
                                return mr}) nodes
}
    
runtime.Run <@ readNode nodes @>

*)