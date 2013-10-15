open Nessos.MBrace.Actors
open Nessos.MBrace.Store
open Nessos.MBrace.Client
open Nessos.MBrace.Utils
open Nessos.MBrace.Lib
open Nessos.MBrace.Lib.MapHashRepeat

[<EntryPoint>]
let main argv =   
    MBraceSettings.MBracedExecutablePath <- @"C:\Program Files (x86)\MBrace\bin\mbraced.exe"              
    MBraceSettings.StoreProvider <- LocalFS
    let runtime = MBrace.InitLocal 4
    //let nodes = runtime.Run <@ createNodes 6 @>
    0 // return an integer exit code
