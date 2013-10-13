open Nessos.MBrace.Actors
open Nessos.MBrace.Store
open Nessos.MBrace.Client
open Nessos.MBrace.Utils
open Nessos.MBrace.Lib
open Nessos.MBrace.Lib.MapHashRepeat

[<EntryPoint>]
let main argv =                 
    let runtime = MBrace.InitLocal 4
    0 // return an integer exit code
