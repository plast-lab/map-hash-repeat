//map-rehash-repeat "average computation" in F#

//id,new value, old value
type Node<'Id,'newV,'oldV> = | N of ('Id*'newV*'oldV) ref
    
//creates num Nodes with random new value (1..10)                   
let createNodes num = 
    let rnd = System.Random()    
    List.init num (fun n -> N(ref (n,rnd.Next (1,11),0)))
    //test with mbrace//List.init num (fun n -> N(ref (n,n*n+1,0)))
    
//manual
(*
let createNeighbors (nodes : Node<'Id,'newV,'oldV> list)=     
    [
        (nodes.[0],Set.empty.Add(nodes.[1]).Add(nodes.[2]));
        (nodes.[1],Set.empty.Add(nodes.[0]).Add(nodes.[3]));
        (nodes.[2],Set.empty.Add(nodes.[0]).Add(nodes.[4]));    
        (nodes.[3],Set.empty.Add(nodes.[1]));
        (nodes.[4],Set.empty.Add(nodes.[2]))
    ]
    |> Map.ofList
*)

let createNeighbors (nodes : Node<'Id,'newV,'oldV> list) (nList : List<int*int>) = 
    [| for n in nList ->
        match n with
            | (fromN,toN) ->
                (nodes.[fromN],nodes.[toN])
    |] 
    |> Seq.groupBy fst |> Seq.map (fun (key,values) -> (key,values |> Seq.map (fun (k1,v1) -> v1) |> Set.ofSeq)) 
    |> Seq.toArray 
    |> Map.ofArray
    
    

//average
let compute (vals : 'a list) = 
    List.sum vals / vals.Length


let rec mapRehashRepeat (nodes : Node<'Id,'newV,'oldV> list) 
                    (neighbors : Map<Node<'Id,'newV,'oldV>,Set<Node<'Id,'newV,'oldV>>>) 
                    compute isDone finish =
    //get the neighbors of the given node                             
    let receive (node : Node<'Id,'newV,'oldV>) = 
            neighbors.[node]
    
    //sets the new value of the given node changes to the average. old value is the previous new value
    let changeV (node : Node<'Id,'newV,'oldV>) comp =     
        match node with
            |N(data) -> 
                let (id,currentV,oldv) = !data
                data := (id,comp,currentV)   
    
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

let finish = ref true     
let nodes = createNodes 6
let neighbors = createNeighbors nodes [(0,1);(1,0);(1,2);(2,1);(1,3);(3,1);(2,4);(4,2);(3,4);(4,3);(4,5);(5,4)]
mapRehashRepeat nodes neighbors compute (fun node comp -> match node with
                                                            |N(data) -> 
                                                                let (id,currentV,oldv) = !data
                                                                currentV = comp) finish

