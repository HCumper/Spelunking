(* Executes intents against sessions, including modal flow, save/load, and turn-taking actions. *)
module Spelunk.SessionActions

open System
open Spelunk.Application
open Spelunk.Domain
open Spelunk.Messages
open Spelunk.Model
open Spelunk.Output
open Spelunk.Services

let private outputEvents previousSession nextSession =
    let openedModal =
        match previousSession.Modal, nextSession.Modal with
        | NoModal, LookMode _
        | NoModal, TargetMode _
        | NoModal, InventoryMode
        | NoModal, TimeShiftPrompt _
        | NoModal, QuitConfirm -> true
        | _ -> false

    let closedModal =
        match previousSession.Modal, nextSession.Modal with
        | LookMode _, NoModal
        | TargetMode _, NoModal
        | InventoryMode, NoModal
        | TimeShiftPrompt _, NoModal
        | QuitConfirm, NoModal -> true
        | _ -> false

    combine previousSession.State.Messages nextSession.State.Messages openedModal closedModal

let private targetInRange target state =
    let dx = target.X - state.Player.Position.X
    let dy = target.Y - state.Player.Position.Y
    max (abs dx) (abs dy) <= state.PlayerWeapon.Range

let private moveCursor mapWidth mapHeight dx dy cursor =
    { X = max 0 (min (mapWidth - 1) (cursor.X + dx))
      Y = max 0 (min (mapHeight - 1) (cursor.Y + dy)) }

let private monsterAt point state =
    state.Monsters |> List.tryFind (fun monster -> monster.Position = point)

let private isInterestingTile point state =
    match monsterAt point state with
    | Some _ -> true
    | None ->
        match state.Map.Tiles[point.Y, point.X] with
        | Floor -> false
        | Wall
        | StairsDown -> true

let private firstVisibleInterestingTileInDirection dx dy state =
    let origin = state.Player.Position

    let rec walk point =
        let nextPoint =
            { X = point.X + dx
              Y = point.Y + dy }

        let inBounds =
            nextPoint.X >= 0
            && nextPoint.X < state.Map.Width
            && nextPoint.Y >= 0
            && nextPoint.Y < state.Map.Height

        if not inBounds then
            None
        elif not state.VisibleTiles[nextPoint.Y, nextPoint.X] then
            None
        elif isInterestingTile nextPoint state then
            Some nextPoint
        else
            walk nextPoint

    walk origin

let private applyAction command session =
    let withHistory = SessionHistory.recordAction command session

    { withHistory with
        State = update command withHistory.State }

let private saveCurrentSession services session =
    try
        services.SaveGame(session.State, session.History)

        { session with
            State = addMessage "Game saved." session.State
            Modal = NoModal }
    with ex ->
        { session with
            State = addMessage $"Save failed: {ex.Message}" session.State
            Modal = NoModal }

let private loadSavedSession services session =
    try
        match services.TryLoadGame () with
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

let applyIntent services intent session : Transition =
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
                saveCurrentSession services session
            | NoModal, LoadGame ->
                loadSavedSession services session
            | NoModal, OpenLook ->
                { session with Modal = LookMode session.State.Player.Position }
            | NoModal, OpenTarget ->
                { session with Modal = TargetMode session.State.Player.Position }
            | NoModal, OpenInventory ->
                { session with Modal = InventoryMode }
            | LookMode cursor, MoveCursor (dx, dy) ->
                let nextSession =
                    match firstVisibleInterestingTileInDirection dx dy session.State with
                    | Some nextCursor ->
                        { session with Modal = LookMode nextCursor }
                    | None ->
                        { session with
                            State = addMessage "You can see nothing there." session.State
                            Modal = LookMode cursor }

                nextSession
            | LookMode _, Confirm
            | LookMode _, Cancel ->
                { session with Modal = NoModal }
            | TargetMode cursor, MoveCursor (dx, dy) ->
                { session with Modal = TargetMode(moveCursor mapWidth mapHeight dx dy cursor) }
            | TargetMode cursor, Confirm ->
                let updatedSession =
                    if targetInRange cursor session.State then
                        applyAction (FireAt cursor) session
                    else
                        { session with
                            State =
                                addMessage
                                    (sprintf "%s cannot reach that far." session.State.PlayerWeapon.Name)
                                    session.State }

                { updatedSession with Modal = NoModal }
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
                    SessionHistory.rewindSession turns session
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
