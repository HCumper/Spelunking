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
    | TimeShifterKey
    | SaveKey
    | LoadKey
    | DigitKey of int
    | BackspaceKey
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
      { Key = TimeShifterKey; Intent = UseTimeShifter }
      { Key = SaveKey; Intent = SaveGame }
      { Key = LoadKey; Intent = LoadGame }
      { Key = BackspaceKey; Intent = EraseDigit }
      { Key = DigitKey 0; Intent = EnterDigit 0 }
      { Key = DigitKey 1; Intent = EnterDigit 1 }
      { Key = DigitKey 2; Intent = EnterDigit 2 }
      { Key = DigitKey 3; Intent = EnterDigit 3 }
      { Key = DigitKey 4; Intent = EnterDigit 4 }
      { Key = DigitKey 5; Intent = EnterDigit 5 }
      { Key = DigitKey 6; Intent = EnterDigit 6 }
      { Key = DigitKey 7; Intent = EnterDigit 7 }
      { Key = DigitKey 8; Intent = EnterDigit 8 }
      { Key = DigitKey 9; Intent = EnterDigit 9 }
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
