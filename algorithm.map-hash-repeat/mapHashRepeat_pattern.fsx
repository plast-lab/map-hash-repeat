//the map-hash-repeat cloud programming pattern (function only)
[<Cloud>]
let rec mapHashRepeat (input1 : IMutableCloudRef<Node<'Id,'newV,'oldV>> []) 
                      (input2 : IMutableCloudRef<Node<'Id,'newV,'oldV>> []) 
                      (computeValues : (IMutableCloudRef<Node<'Id,'newV,'oldV>> [] ->
                                     IMutableCloudRef<Node<'Id,'newV,'oldV>> [] ->
                                     ICloud<Map<'I,'C>>))
                      createNewTopology
                      isDone  = cloud {              
   
    let keepPrevState (node : IMutableCloudRef<Node<'Id,'newV,'oldV>> ) newV = cloud {
        let! cloudNode = MutableCloudRef.Read(node)
        match cloudNode with        
            | N(v1,v2,oldSet,currentV)  ->                
                let newData = (v1,v2,currentV,newV)                 
                do! MutableCloudRef.Force(node,N(newData))                
    }    
    
    let! newValues = computeValues input1 input2

    let distribute =         
        Array.map (fun input -> cloud {
            let! cloudInput = MutableCloudRef.Read(input)
            match cloudInput with
                | N(v1,v2,oldV,newV)  ->                 
                    do! MutableCloudRef.Force(input,N(v1,newValues.[v1],oldV,newV))
        }) input2

    let! _ = distribute |> Cloud.Parallel 
                   
    let! newTopology = createNewTopology input1 input2    
    let checkAll = 
        [|
            for input in input2 -> cloud {                   
                let! cloudInput = MutableCloudRef.Read(input)
                match cloudInput with   
                    | N(v,_,_,_) when Map.containsKey (v) newTopology ->
                        let comp = newTopology.[v]
                        let! ok = isDone input comp
                        do! keepPrevState input comp  
                        return ok  
            }
        |]  
             
    let! check = Cloud.Parallel checkAll 
    let fixpoint = check |> Seq.fold (fun acc item -> acc && item) true
    match fixpoint with  
        | true -> return (input1,input2)
        | false -> 
            return! mapHashRepeat input1 input2 computeValues computeNeighbors isDone    
}