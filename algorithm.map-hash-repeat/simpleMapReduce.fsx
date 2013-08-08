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

let refs = [ref1;ref2;ref3;ref4;ref5]

let fn x = x * x

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


[<Cloud>]
let mapSeq (fn : int -> int) (refs : ICloudRef<Node<'T>> list) =   
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



#time
let sum1 = runtime.Run <@ mapParallel fn refs @>
let sum2 = runtime.Run <@ mapSeq fn refs @>
