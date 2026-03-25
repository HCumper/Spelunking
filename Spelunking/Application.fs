(* Defines session, modal, intent, and transition types shared across application-level modules. *)
module Spelunk.Application

open Spelunk.Model
open Spelunk.Domain
open Spelunk.Output

type Modal =
    | NoModal
    | LookMode of cursor: Position
    | TargetMode of cursor: Position
    | InventoryMode
    | TimeShiftPrompt of turnsText: string
    | QuitConfirm

type Session =
    { State: GameState
      Modal: Modal
      History: GameState list }

type Intent =
    | Act of Command
    | OpenLook
    | OpenTarget
    | OpenInventory
    | UseTimeShifter
    | SaveGame
    | LoadGame
    | EnterDigit of int
    | EraseDigit
    | MoveCursor of dx: int * dy: int
    | Confirm
    | Cancel
    | Quit
    | Ignore

type Transition =
    { NextSession: Session option
      Events: OutputEvent list }

let initialSession () =
    { State = initialState ()
      Modal = NoModal
      History = [] }

let normalizeIntent session intent =
    match session.Modal, intent with
    | NoModal, MoveCursor (dx, dy) -> Act(Move(dx, dy))
    | _, other -> other
