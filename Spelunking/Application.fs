(* Translates player intents into modal UI state changes and domain commands. *)
module Spelunk.Application

open System
open Spelunk.Config
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

let private outputEvents previousSession nextSession =
    let openedModal =
        match previousSession.Modal, nextSession.Modal with
        | NoModal, LookMode _
        | NoModal, TargetMode _
        | NoModal, InventoryMode -> true
        | NoModal, TimeShiftPrompt _ -> true
        | NoModal, QuitConfirm -> true
        | _ -> false

    let closedModal =
        match previousSession.Modal, nextSession.Modal with
        | LookMode _, NoModal
        | TargetMode _, NoModal
        | InventoryMode, NoModal -> true
        | TimeShiftPrompt _, NoModal -> true
        | QuitConfirm, NoModal -> true
        | _ -> false

    combine previousSession.State.Messages nextSession.State.Messages openedModal closedModal

let normalizeIntent session intent =
    match session.Modal, intent with
    | NoModal, MoveCursor (dx, dy) -> Act(Move(dx, dy))
    | _, other -> other

let private addMessage message state =
    { state with Messages = message :: state.Messages |> List.truncate 6 }

let private targetInRange target state =
    let dx = target.X - state.Player.Position.X
    let dy = target.Y - state.Player.Position.Y
    max (abs dx) (abs dy) <= state.PlayerWeapon.Range

let private moveCursor mapWidth mapHeight dx dy cursor =
    { X = max 0 (min (mapWidth - 1) (cursor.X + dx))
      Y = max 0 (min (mapHeight - 1) (cursor.Y + dy)) }

let private recordsHistory command =
    match command with
    | Move _
    | Wait
    | FireAt _ -> true

let private applyAction command session =
    let history =
        if recordsHistory command then
            session.State :: session.History |> List.truncate 10
        else
            session.History

    { session with
        State = update command session.State
        History = history }

let private rewindSession turns session =
    let availableTurns = session.History.Length
    let actualTurns = max 1 (min turns availableTurns)

    match session.History |> List.tryItem (actualTurns - 1) with
    | Some priorState ->
        { session with
            State =
                { priorState with
                    Messages =
                        sprintf "The time shifter drags you back %d turn%s." actualTurns (if actualTurns = 1 then "" else "s")
                        :: priorState.Messages
                        |> List.truncate 6 }
            Modal = NoModal
            History = session.History |> List.skip actualTurns }
    | None ->
        { session with
            State = addMessage "The time shifter finds no earlier state." session.State
            Modal = NoModal }

let private saveCurrentSession session =
    try
        Spelunk.Save.saveGame session.State session.History

        { session with
            State = addMessage "Game saved." session.State
            Modal = NoModal }
    with ex ->
        { session with
            State = addMessage $"Save failed: {ex.Message}" session.State
            Modal = NoModal }

let private loadSavedSession session =
    try
        match Spelunk.Save.tryLoadGame () with
        | Some (loadedState, loadedHistory) ->
            { State = addMessage "Game loaded." loadedState
              Modal = NoModal
              History = loadedHistory }
        | None ->
            { session with
                State = addMessage "No saved game found." session.State
                Modal = NoModal }
    with ex ->
        { session with
            State = addMessage $"Load failed: {ex.Message}" session.State
            Modal = NoModal }

let applyIntent intent session : Transition =
    match intent with
    | Ignore -> { NextSession = Some session; Events = [] }
    | _ ->
        let mapWidth = session.State.Map.Width
        let mapHeight = session.State.Map.Height

        let nextSession =
            match session.Modal, intent with
            | NoModal, Quit ->
                { session with Modal = QuitConfirm }
            | QuitConfirm, Confirm ->
                session
            | QuitConfirm, Cancel
            | QuitConfirm, Quit ->
                { session with Modal = NoModal }
            | NoModal, Act command ->
                applyAction command session
            | NoModal, UseTimeShifter ->
                { session with Modal = TimeShiftPrompt "" }
            | NoModal, SaveGame ->
                saveCurrentSession session
            | NoModal, LoadGame ->
                loadSavedSession session
            | NoModal, OpenLook ->
                { session with Modal = LookMode session.State.Player.Position }
            | NoModal, OpenTarget ->
                { session with Modal = TargetMode session.State.Player.Position }
            | NoModal, OpenInventory ->
                { session with Modal = InventoryMode }
            | LookMode cursor, MoveCursor (dx, dy) ->
                { session with Modal = LookMode(moveCursor mapWidth mapHeight dx dy cursor) }
            | LookMode _, Confirm
            | LookMode _, Cancel ->
                { session with Modal = NoModal }
            | TargetMode cursor, MoveCursor (dx, dy) ->
                { session with Modal = TargetMode(moveCursor mapWidth mapHeight dx dy cursor) }
            | TargetMode cursor, Confirm ->
                let rewoundOrUpdatedSession =
                    if targetInRange cursor session.State then
                        // Target mode is an aiming UI; the actual shot resolves through the normal command pipeline.
                        applyAction (FireAt cursor) session
                    else
                        { session with
                            State =
                                addMessage
                                    (sprintf "%s cannot reach that far." session.State.PlayerWeapon.Name)
                                    session.State }

                { rewoundOrUpdatedSession with Modal = NoModal }
            | TargetMode _, Cancel ->
                { session with Modal = NoModal }
            | InventoryMode, Confirm
            | InventoryMode, Cancel ->
                { session with Modal = NoModal }
            | TimeShiftPrompt turnsText, EnterDigit digit ->
                if turnsText.Length >= 2 then
                    session
                else
                    { session with Modal = TimeShiftPrompt(turnsText + string digit) }
            | TimeShiftPrompt turnsText, EraseDigit ->
                let nextText =
                    if String.IsNullOrEmpty turnsText then
                        ""
                    else
                        turnsText.Substring(0, turnsText.Length - 1)

                { session with Modal = TimeShiftPrompt nextText }
            | TimeShiftPrompt turnsText, Confirm ->
                match Int32.TryParse turnsText with
                | true, turns when turns >= 1 ->
                    rewindSession turns session
                | _ ->
                    { session with
                        State = addMessage "Enter a turn count from 1 to 10." session.State
                        Modal = NoModal }
            | TimeShiftPrompt _, Cancel ->
                { session with Modal = NoModal }
            | _, _ ->
                session

        let nextSessionOption =
            match session.Modal, intent with
            | QuitConfirm, Confirm -> None
            | _ -> Some nextSession

        { NextSession = nextSessionOption
          Events =
            match nextSessionOption with
            | Some resolved -> outputEvents session resolved
            | None -> [] }
