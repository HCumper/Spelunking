(* Browser persistence boundary. This is intentionally thin so localStorage or import/export can be added without touching game rules. *)
module Spelunking.Web.Storage

open System
open Microsoft.JSInterop
open Spelunk.Model

let private storageKey = "spelunk.savegame"

let mutable private runtime: IJSInProcessRuntime option = None

let configure (js: IJSRuntime) =
    match js with
    | :? IJSInProcessRuntime as syncRuntime -> runtime <- Some syncRuntime
    | _ -> runtime <- None

let saveGame (state: GameState, history: GameState list) =
    match runtime with
    | Some js ->
        let json = Spelunk.Save.serializeSession state history
        js.Invoke<obj>("localStorage.setItem", storageKey, json) |> ignore
    | None -> ()

let tryLoadGame () : (GameState * GameState list) option =
    match runtime with
    | Some js ->
        try
            let json = js.Invoke<string>("localStorage.getItem", storageKey)

            if String.IsNullOrWhiteSpace json then
                None
            else
                Spelunk.Save.deserializeSession json
        with _ ->
            None
    | None -> None
