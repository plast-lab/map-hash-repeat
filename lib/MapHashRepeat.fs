namespace Nessos.MBrace.Lib
    
    module MapHashRepeat =

        open Nessos.MBrace.Client

        type Node<'Id,'newV,'oldV when 'Id : comparison > = | N of 'Id*'newV*'oldV* Set<'Id>
        
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
        let createNodes (num : int) = cloud {    
            let rnd = System.Random() 
            let initVals = [| for n in 0 .. num-1 -> cloud { 
                                    //let node = (n, rnd.Next (1,11), 0, Set.empty)  
                                    let node = (n,n*n+1,0,Set.empty)        //test with f#
                                    return! MutableCloudRef.New(N(node)) 
                                    }
                            |]
            return! Cloud.Parallel initVals
        } 

        //each node contains its neighbors' ids
        [<Cloud>]
        let createNeighbors (nodes : IMutableCloudRef<Node<'Id,'newV,'oldV>> []) (nArray :  ('id*'id) []) = cloud   {    
            for n in nArray do
                match n with
                    | (parent,neighbor) ->
                        for node in nodes do
                        let! cloudNode = MutableCloudRef.Read(node)
                        match cloudNode with   
                            | N(id,newv,oldv,setN) when parent = id ->                                          
                                do! MutableCloudRef.Force(node,N(id,newv,oldv,setN.Add(neighbor))) 
                            | N(id,newv,oldv,setN) -> ()
        }
    

        (*
        let createNeighbors (nodes : IMutableCloudRef<Node<'Id,'newV,'oldV>> []) (nList : List<int*int>) = 
            [| for n in nList ->
                match n with
                    | (parent,neighbor) ->
                        (parent,neighbor)
            |] 
            |> Seq.groupBy fst |> Seq.map (fun (key,values) -> (key,values |> Seq.map (fun (k1,v1) -> v1) |> Set.ofSeq)) 
            |> Seq.toArray 
            |> Map.ofArray
        *)


        //average
        let compute (vals : 'a list) = 
            let rec fib n =
                match n with
                | 1 | 2 -> 1
                | n -> fib(n-1) + fib(n-2)
            let _ = fib 45 
            List.sum vals / vals.Length


        [<Cloud>]
        let rec mapHashRepeat (nodes : IMutableCloudRef<Node<'Id,'newV,'oldV>> []) 
                                //(neighbors : Map<'nodeId,Set<'nodeId>>) 
                                compute isDone  = cloud {              
   
            //sets the new value of the given node changes to the average. old value is the previous new value
            let changeV (node : IMutableCloudRef<Node<'Id,'newV,'oldV>> ) comp = cloud {
                let! cloudNode = MutableCloudRef.Read(node)
                match cloudNode with        
                    | N(id,currentV,oldv,setN)  ->                
                        let newData = (id,comp,currentV,setN)                 
                        do! MutableCloudRef.Force(node,N(newData))                
            }

            let newVals (node : IMutableCloudRef<Node<'Id,'newV,'oldV>> ) = cloud {
        
                //get the neighbors REFS of the given node                             
                let getNeighbors (node : IMutableCloudRef<Node<'Id,'newV,'oldV>> ) = cloud {
                    let! cloudNode = MutableCloudRef.Read(node)
                    match cloudNode with
                        | N(_,_,_,setN) ->    
                            let neighborIds = Set.toList setN
                            return [for n in neighborIds -> nodes.[n]]
                
                } 
                //gets the newv from the given node
                let getNewV (node : IMutableCloudRef<Node<'Id,'newV,'oldV>> ) = cloud {
                        let! cloudNode = MutableCloudRef.Read(node)
                        match cloudNode with
                            | N(_,newv,_,_)  ->                 
                                return newv    
                }
                //get neighbors REFS from the given node
                let! neighborRefs = getNeighbors node   

                //concatenate given node's newv with the neighbors' newv and return a list with tuples: (nodeId,newvs)
                let! cloudNode = MutableCloudRef.Read(node)
                match cloudNode with   
                    | N(id,_,_,_) ->
                        let! neighborVals = seqMap getNewV neighborRefs 
                        let! myVal = getNewV node
                        return (id, [myVal] @ neighborVals)
            }
            //the newv of the neighbors
            let vals = [|for node in nodes -> newVals node|] 
            let! results = Cloud.Parallel vals 
            let newVals = results |> Map.ofArray
            let av = [| for node in nodes ->
                        cloud {
                            let! cloudNode = MutableCloudRef.Read(node)
                            match cloudNode with   
                            | N(id,_,_,_) ->
                                let vals = newVals.[id]
                                return (id,compute vals)
                        } 
                    |]   
            let! averagesArr = Cloud.Parallel av
            let averages = averagesArr |> Map.ofArray
    
            let checkAll = 
                [|
                    for node in nodes -> cloud {                   
                        let! cloudNode = MutableCloudRef.Read(node)
                        match cloudNode with   
                            | N(id,_,_,_) when Map.containsKey id averages ->
                                let comp = averages.[id]
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
    


