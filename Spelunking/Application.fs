module Spelunk.Application

open Spelunk.Domain

type Modal =
    | NoModal
    | LookMode of cursor: Point
    | TargetMode of cursor: Point
    | InventoryMode

type Session =
    { State: GameState
      Modal: Modal }

type OverlayColor =
    | Default
    | Inverted
    | Highlight

type OverlayCursorStyle =
    | InspectCursor
    | TargetCursor

type OverlayPanelStyle =
    | FullWidthFooter
    | CenterDialog

type OverlayCursor =
    { Position: Point
      Style: OverlayCursorStyle
      Foreground: OverlayColor
      Background: OverlayColor }

type OverlayPanel =
    { Style: OverlayPanelStyle
      Title: string
      Lines: string list }

type OverlayViewModel =
    { Cursor: OverlayCursor option
      Panel: OverlayPanel option }

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

type Key =
    | Up
    | Down
    | Left
    | Right
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

let initialSession () =
    { State = initialState ()
      Modal = NoModal }

let defaultBindings : Binding list =
    [ { Key = QuitKey; Intent = Quit }
      { Key = CancelKey; Intent = Cancel }
      { Key = ConfirmKey; Intent = Confirm }
      { Key = LookKey; Intent = OpenLook }
      { Key = TargetKey; Intent = OpenTarget }
      { Key = InventoryKey; Intent = OpenInventory }
      { Key = Up; Intent = MoveCursor(0, -1) }
      { Key = Down; Intent = MoveCursor(0, 1) }
      { Key = Left; Intent = MoveCursor(-1, 0) }
      { Key = Right; Intent = MoveCursor(1, 0) }
      { Key = WaitKey; Intent = Act Wait } ]

let intentFromKey bindings key =
    bindings
    |> List.tryFind (fun binding -> binding.Key = key)
    |> Option.map (fun binding -> binding.Intent)
    |> Option.defaultValue Ignore

let normalizeIntent session intent =
    match session.Modal, intent with
    | NoModal, MoveCursor (dx, dy) -> Act(Move(dx, dy))
    | _, other -> other

let private addMessage message state =
    { state with Messages = message :: state.Messages |> List.truncate 6 }

let private moveCursor mapWidth mapHeight dx dy cursor =
    { X = max 0 (min (mapWidth - 1) (cursor.X + dx))
      Y = max 0 (min (mapHeight - 1) (cursor.Y + dy)) }

let private monsterAt point state =
    state.Monsters |> List.tryFind (fun monster -> monster.Position = point)

let private targetInRange target state =
    let dx = target.X - state.Player.Position.X
    let dy = target.Y - state.Player.Position.Y
    max (abs dx) (abs dy) <= 8

let private monsterDescription point state =
    state.Monsters |> List.tryFind (fun monster -> monster.Position = point)

let private describePoint point state =
    if point = state.Player.Position then
        "You. The scavenger."
    else
        match monsterDescription point state with
        | Some monster -> sprintf "%s (%c), HP %d/%d." monster.Name monster.Glyph monster.Hp monster.MaxHp
        | None ->
            match state.Map.Tiles[point.Y, point.X] with
            | Wall -> "Rough cavern wall."
            | Floor -> "Open floor."
            | StairsDown -> "A staircase leading deeper."

let private inventoryLines () =
    [ "a) Rusty raygun"
      "b) 2 ration packs"
      "c) Frayed field notebook"
      ""
      "Inventory is UI-only here. Esc closes." ]

let overlayViewModel session =
    let width = session.State.Map.Width
    let mapHeight = session.State.Map.Height

    match session.Modal with
    | NoModal -> None
    | LookMode cursor ->
        Some
            { Cursor =
                Some
                    { Position = cursor
                      Style = InspectCursor
                      Foreground = Inverted
                      Background = Highlight }
              Panel =
                Some
                    { Style = FullWidthFooter
                      Title = "LOOK MODE"
                      Lines =
                        [ describePoint cursor session.State
                          "Move cursor with arrows/WASD. Enter or Esc closes." ] } }
    | TargetMode cursor ->
        let targetText =
            match monsterDescription cursor session.State with
            | Some monster when targetInRange cursor session.State ->
                sprintf "Target locked: %s at (%d,%d)." monster.Name cursor.X cursor.Y
            | Some monster ->
                sprintf "%s is out of range." monster.Name
            | None ->
                sprintf "No target at (%d,%d)." cursor.X cursor.Y

        Some
            { Cursor =
                Some
                    { Position = cursor
                      Style = TargetCursor
                      Foreground = Inverted
                      Background = Highlight }
              Panel =
                Some
                    { Style = FullWidthFooter
                      Title = "TARGET MODE"
                      Lines =
                        [ targetText
                          "Move cursor. Enter confirms. Esc closes." ] } }
    | InventoryMode ->
        Some
            { Cursor = None
              Panel =
                Some
                    { Style = CenterDialog
                      Title = "INVENTORY"
                      Lines = inventoryLines () } }

let applyIntent intent session =
    match intent with
    | Quit -> None
    | Ignore -> Some session
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
                    match monsterAt cursor session.State with
                    | Some monster when targetInRange cursor session.State ->
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

        Some nextSession
