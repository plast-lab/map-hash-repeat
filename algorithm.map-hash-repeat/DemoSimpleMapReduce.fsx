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
                        do! Cloud.OfAsync <| Async.Sleep 1000
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
        let map2 (fn : int -> int) (ref : ICloudRef<Node<'T>>) =  
            //cloud {  
            match ref.Value with
                | N(id) ->
                    async {
                        do! Async.Sleep 1000
                    } |> Async.RunSynchronously
                    fn id
            //}
        let results = [| for ref in refs -> map2 fn ref |]            
        return Array.sum results        
    } 

#time
let sum1 = runtime.Run <@ mapParallel fn refs @>
let sum2 = runtime.Run <@ mapLocal fn refs @>      

[<Cloud>]
let race () =
    cloud {
        let r = ref 0
        let! _ = Cloud.Parallel [| for i in 1 .. 1000 -> cloud { incr r } |]
        return !r
    }  

runtime.Run <@ race () @>

[<Cloud>]
let race2() =
    cloud {
        let x = [1..10]
        let! y,_ = cloud { return x } <||> cloud { return () }
        return obj.ReferenceEquals(x, y)
    }

runtime.Run <@ race2 () @>


[<Cloud>]
let increment (counter : IMutableCloudRef<int>) = 
    cloud {
        let! v = MutableCloudRef.Read counter
        do! MutableCloudRef.Force(counter, v + 1)    
    }

[<Cloud>]
let incBy100 (incMethod : IMutableCloudRef<int> -> ICloud<unit>) =
    cloud {
        let! mutRef = MutableCloudRef.New(0)
        let! res = Cloud.Parallel [| for i in 1 .. 100 -> cloud { return! incMethod mutRef }|]
        return! MutableCloudRef.Read(mutRef)
    }

let proc = runtime.CreateProcess <@  incBy100 increment |> Cloud.Trace @>
runtime.ShowUserLogs proc.ProcessId


[<Cloud>]
let trace () =
    cloud {
        let a, b = 4, 5
        let! (c,d) = cloud { return a + 1 } <||> cloud { return b + 1 }
        return c * d
    }
let proc' = runtime.CreateProcess <@ trace () |> Cloud.Trace @>
// retrieve trace info
runtime.ShowUserLogs proc'.ProcessId

open System.Net

[<Cloud>]
let test () = 
    cloud {
        let http = new System.Net.WebClient()
        let download (url : string)= cloud { return http.DownloadString url }
        return! Cloud.Parallel <| Array.map download [| "http://www.m-brace.net" ; "http://www.nessos.gr" |]
    }

runtime.Run <@ test () @>
runtime.Nodes

[<Cloud>]
let filter (f : 'T -> bool) (xs : 'T []) =
    cloud {
        // a nested subexpression that performs
        // the checking on a single element
        let pick (input : 'T) =
            cloud {
                if f input then return Some input
                else return None
            }

        let! results = Cloud.Parallel <| Array.map pick xs
        return Array.choose id results
    }