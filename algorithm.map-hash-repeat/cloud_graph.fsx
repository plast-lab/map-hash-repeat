
// BEGIN PREAMBLE -- do not evaluate, for intellisense only
#r "Nessos.MBrace.Utils"
#r "Nessos.MBrace.Actors"
#r "Nessos.MBrace.Base"
#r "Nessos.MBrace.Store"
#r "Nessos.MBrace.Client"

open Nessos.MBrace.Client
// END PREAMBLE

#r "../lib/bin/Debug/Nessos.MBrace.Lib.dll"
open Nessos.MBrace.Lib
open Nessos.MBrace.Lib.MapReduce

type Node = N of int * List<int>

type Graph = 
    | G of List<ICloudRef<Node>>
    | Empty

//gets a Node and returns a CloudRef Node
[<Cloud>]
let CloudNode (node : int * List<int>)  =
    cloud {
        return! newRef <| N(node)
    }  
    
//insert node to graph (cloud)
[<Cloud>]
let insert (g : ICloudRef<Graph>) (n : ICloudRef<Node>) =   
    cloud {  
        match g.Value with 
        | G(nodes) -> return! newRef <| G(List.append nodes [n])
        | Empty ->  return! newRef <| G([n])
    }

[<Cloud>]
let graph = 
    cloud {
        return! newRef <| Empty
    }

         
[<Cloud>]
let createGraph createNode insert graph = 
    cloud {
        let! n = createNode (0,[])  
        return! insert graph n
    }
    
// create a local-only runtime
let runtime = MBrace.InitLocal 4

// upload & execute
let proc = runtime.CreateProcess <@ createGraph CloudNode insert graph @> 

let graphCloudRef = proc.AwaitResult()
