//map -rehash-repeat framework in F#
type Node<'I> =  | N of 'I
                  
let createNodes num = [|for i in 0..num -> N(i)|] |> Array.toList
let nodes = createNodes 5       //apo 0 mexri kai 5

//Map<IState,Set<IStates>
let neighbors =     
    [
        (nodes.[0],Set.empty.Add(nodes.[1]).Add(nodes.[3]));
        (nodes.[1],Set.empty);
        (nodes.[2],Set.empty.Add(nodes.[1]));    
        (nodes.[3],Set.empty.Add(nodes.[4]).Add(nodes.[1]));
        (nodes.[4],Set.empty.Add(nodes.[1]).Add(nodes.[2]));
        (nodes.[5],Set.empty)
    ]
    |> Map.ofList


//Map<IState,Set<V>> neighborVals
  

open System
open System.IO

let fileSource = Path.Combine(__SOURCE_DIRECTORY__, @"Wikipedia")
let files = Directory.GetFiles(fileSource) |> Seq.toArray
let texts = files |> Array.map (fun path -> File.ReadAllText (path))
     
//Map<IState,Set<V>>
let init (nodes : Node<'I> list) (texts : string []) =           
    [for text in texts ->         
        let pos = text.Length % nodes.Length
        printfn "node %d ---> %s" pos text
        (nodes.[pos],text)
    ] 
    |> Seq.groupBy fst |> Seq.map (fun (key,values) -> (key,values |> Seq.map (fun (k1,v1) -> v1) |> Set.ofSeq)) 
    |>  Seq.toArray 
    |> Map.ofArray
    //for v in vals do printfn "%A" v          
        
let initVals = init nodes texts


let compute (texts : string[]) =
        let words = texts |> Array.map (fun text -> text.Split([|' '; '.'; ','|], StringSplitOptions.RemoveEmptyEntries)) |> Seq.concat
        words
        |> Seq.map (fun word -> word.ToLower())
        |> Seq.map (fun t -> t.Trim())
        |> Seq.filter (fun word -> Seq.length word > 3 )
        |> Seq.groupBy id
        |> Seq.map (fun (key, values) -> (key, values |> Seq.length))
        |> Seq.sortBy (fun (_,t) -> -t)
        |> Seq.toArray

//boolean done = true
//do {
let mapRehashRepeat (nodes : Node<'I> list) (initVals : Map<Node<'I>,Set<'S>>) (neighbors : Map<Node<'I>,Set<Node<'I>>>) (compute : (string [] -> (string * int) []))=                
    for node in nodes do 
        //printfn "%A" node                       
        if Map.containsKey node initVals then 
            let vals = initVals.[node] |> Set.toArray
            let computations = compute vals             //V v = s.compute()
            //printfn "%A" computations
            //if (!v.isDone())
                //done = false
            //neighbors[s]. = computeNeighbors(s,v)
            //printfn "neighbors %A" neighbors.[node]
            let neighbors = neighbors.[node] |> Set.toArray            
            let neighborVals = [|for node in nodes -> (node,Set.empty<(string * int)[]>)|] |> Map.ofArray
            for n in neighbors do
                let _ = neighborVals.[n].Add(computations)
                ()                                
                                           
//while (!done)

mapRehashRepeat nodes initVals neighbors compute
