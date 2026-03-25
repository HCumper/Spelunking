module Spelunk.Application

open Spelunk.Config
open Spelunk.Model
open Spelunk.Domain
open Spelunk.Output

type Modal =
    | NoModal
    | LookMode of cursor: Position
    | TargetMode of cursor: Position
    | InventoryMode
    | QuitConfirm

type Session =
    { State: GameState
      Modal: Modal }

type Intent =
    | Act of Command
    | OpenLook
    | OpenTarget
    | OpenInventory
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
      Modal = NoModal }

let private outputEvents previousSession nextSession =
    let openedModal =
        match previousSession.Modal, nextSession.Modal with
        | NoModal, LookMode _
        | NoModal, TargetMode _
        | NoModal, InventoryMode -> true
        | NoModal, QuitConfirm -> true
        | _ -> false

    let closedModal =
        match previousSession.Modal, nextSession.Modal with
        | LookMode _, NoModal
        | TargetMode _, NoModal
        | InventoryMode, NoModal -> true
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
                { session with State = update command session.State }
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
                let nextState =
                    if targetInRange cursor session.State then
                        update (FireAt cursor) session.State
                    else
                        addMessage
                            (sprintf "%s cannot reach that far." session.State.PlayerWeapon.Name)
                            session.State

                { State = nextState; Modal = NoModal }
            | TargetMode _, Cancel ->
                { session with Modal = NoModal }
            | InventoryMode, Confirm
            | InventoryMode, Cancel ->
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
