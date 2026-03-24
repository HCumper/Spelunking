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
        | _ -> false

    let closedModal =
        match previousSession.Modal, nextSession.Modal with
        | LookMode _, NoModal
        | TargetMode _, NoModal
        | InventoryMode, NoModal -> true
        | _ -> false

    combine previousSession.State.Messages nextSession.State.Messages openedModal closedModal

let normalizeIntent session intent =
    match session.Modal, intent with
    | NoModal, MoveCursor (dx, dy) -> Act(Move(dx, dy))
    | _, other -> other

let private addMessage message state =
    { state with Messages = message :: state.Messages |> List.truncate 6 }

let private moveCursor mapWidth mapHeight dx dy cursor =
    { X = max 0 (min (mapWidth - 1) (cursor.X + dx))
      Y = max 0 (min (mapHeight - 1) (cursor.Y + dy)) }

let applyIntent intent session : Transition =
    match intent with
    | Quit -> { NextSession = None; Events = [] }
    | Ignore -> { NextSession = Some session; Events = [] }
    | _ ->
        let mapWidth = session.State.Map.Width
        let mapHeight = session.State.Map.Height

        let nextSession =
            match session.Modal, intent with
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
                    let dx = cursor.X - session.State.Player.Position.X
                    let dy = cursor.Y - session.State.Player.Position.Y
                    let inRange = max (abs dx) (abs dy) <= (combatSettings ()).TargetRange

                    match session.State.Monsters |> List.tryFind (fun monster -> monster.Position = cursor) with
                    | Some monster when inRange ->
                        addMessage (sprintf "You line up a shot on the %s." monster.Name) session.State
                    | Some monster ->
                        addMessage (sprintf "The %s is out of range." monster.Name) session.State
                    | None ->
                        addMessage "No target there." session.State

                { State = nextState; Modal = NoModal }
            | TargetMode _, Cancel ->
                { session with Modal = NoModal }
            | InventoryMode, Confirm
            | InventoryMode, Cancel ->
                { session with Modal = NoModal }
            | _, _ ->
                session

        { NextSession = Some nextSession
          Events = outputEvents session nextSession }
