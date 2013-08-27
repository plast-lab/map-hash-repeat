// BEGIN PREAMBLE -- do not evaluate, for intellisense only
#r "Nessos.MBrace.Utils"
#r "Nessos.MBrace.Actors"
#r "Nessos.MBrace.Base"
#r "Nessos.MBrace.Store"
#r "Nessos.MBrace.Client"

#r "../lib/bin/Debug/Nessos.MBrace.Lib.dll"

open Nessos.MBrace.Client

type Node<'T> = N of 'T

[<Cloud>]
let cloudNode value = 
    cloud {
        return! newRef <| N(value)
    }

let runtime = MBrace.InitLocal 4


let ref1 = runtime.Run <@ cloudNode 1 @>
let ref2 = runtime.Run <@ cloudNode 2 @>
let ref3 = runtime.Run <@ cloudNode 3 @>
let ref4 = runtime.Run <@ cloudNode 4 @>
let ref5 = runtime.Run <@ cloudNode 5 @>
let ref6 = runtime.Run <@ cloudNode 6 @>
let ref7 = runtime.Run <@ cloudNode 7 @>
let ref8 = runtime.Run <@ cloudNode 8 @>
let ref9 = runtime.Run <@ cloudNode 9 @>
let ref10 = runtime.Run <@ cloudNode 10 @>

let refs = [ref1;ref2;ref3;ref4;ref5;ref6;ref7;ref8;ref9;ref10]

let fn x = x * x

//parallel
[<Cloud>]
let mapParallel (fn : int -> int) (refs : ICloudRef<Node<'T>> list) =   
    cloud {
        let map2 (fn : int -> int) (ref : ICloudRef<Node<'T>>) =  
            cloud {  
                match ref.Value with
                    | N(id) -> 
                        do! Cloud.OfAsync <| Async.Sleep 3000
                        return fn id
            }
        let jobs = [| for ref in refs -> map2 fn ref |]                   
        let! results = Cloud.Parallel jobs        
        return Array.sum results
    } 


//single worker node
[<Cloud>]
let mapLocal (fn : int -> int) (refs : ICloudRef<Node<'T>> list) =   
     cloud {
        let rec map2 (fn : int -> int) (refs : ICloudRef<Node<'T>> list) total =  
            cloud {  
                match refs with
                    | [] -> 
                        return total
                    | hd :: tl ->
                        match hd.Value with
                            | N(id) -> 
                                do! Cloud.OfAsync <| Async.Sleep 3000
                                let ntotal = (fn id) + total
                                return! map2 fn tl ntotal
            }       
        let! result = map2 fn refs 0
        return result        
    } 

#time
let sum1 = runtime.Run <@ mapParallel fn refs @>
let sum2 = runtime.Run <@ mapLocal fn refs @>