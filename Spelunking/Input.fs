(* Defines abstract input keys and the default mapping from keys to application intents. *)
module Spelunk.Input

open Spelunk.Application
open Spelunk.Model

type Key =
    | UpLeft
    | Up
    | UpRight
    | Down
    | DownLeft
    | Left
    | Right
    | DownRight
    | WaitKey
    | ConfirmKey
    | CancelKey
    | LookKey
    | TargetKey
    | InventoryKey
    | QuitKey

type Binding =
    { Key: Key
      Intent: Intent }

let defaultBindings : Binding list =
    // Movement keys feed cursor movement first; Application.normalizeIntent turns that into walking when no modal is open.
    [ { Key = QuitKey; Intent = Quit }
      { Key = CancelKey; Intent = Cancel }
      { Key = ConfirmKey; Intent = Confirm }
      { Key = LookKey; Intent = OpenLook }
      { Key = TargetKey; Intent = OpenTarget }
      { Key = InventoryKey; Intent = OpenInventory }
      { Key = UpLeft; Intent = MoveCursor(-1, -1) }
      { Key = Up; Intent = MoveCursor(0, -1) }
      { Key = UpRight; Intent = MoveCursor(1, -1) }
      { Key = DownLeft; Intent = MoveCursor(-1, 1) }
      { Key = Down; Intent = MoveCursor(0, 1) }
      { Key = Left; Intent = MoveCursor(-1, 0) }
      { Key = Right; Intent = MoveCursor(1, 0) }
      { Key = DownRight; Intent = MoveCursor(1, 1) }
      { Key = WaitKey; Intent = Act Wait } ]

let intentFromKey bindings key =
    bindings
    |> List.tryFind (fun binding -> binding.Key = key)
    |> Option.map (fun binding -> binding.Intent)
    |> Option.defaultValue Ignore
