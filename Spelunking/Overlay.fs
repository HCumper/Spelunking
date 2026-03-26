(* Builds transient overlay view models for look mode, target mode, inventory, and quit confirmation. *)
module Spelunk.Overlay

open Spelunk.Model
open Spelunk.Application

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
    { Position: Position
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

let private monsterAt point state =
    state.Monsters |> List.tryFind (fun monster -> monster.Position = point)

let private targetInRange target state =
    let dx = target.X - state.Player.Position.X
    let dy = target.Y - state.Player.Position.Y
    max (abs dx) (abs dy) <= state.PlayerWeapon.Range

let private describePoint point state =
    if point = state.Player.Position then
        "You. The scavenger."
    else
        match monsterAt point state with
        | Some monster -> sprintf "%s (%c), HP %d/%d." monster.Name monster.Glyph monster.Hp monster.MaxHp
        | None ->
            match state.Map.Tiles[point.Y, point.X] with
            | Wall -> "Rough cavern wall."
            | Floor -> "Open floor."
            | Tardis -> "The TARDIS. Entering it will carry you onward."

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
                          "Choose a direction. First visible feature is shown. Enter or Esc closes." ] } }
    | TargetMode cursor ->
        let targetText =
            match monsterAt cursor session.State with
            | Some monster when targetInRange cursor session.State ->
                sprintf "%s ready: %s at (%d,%d)." session.State.PlayerWeapon.Name monster.Name cursor.X cursor.Y
            | Some monster ->
                sprintf "%s is out of range for %s." monster.Name session.State.PlayerWeapon.Name
            | None ->
                sprintf "Aim %s at (%d,%d)." session.State.PlayerWeapon.Name cursor.X cursor.Y

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
    | TimeShiftPrompt turnsText ->
        Some
            { Cursor = None
              Panel =
                Some
                    { Style = CenterDialog
                      Title = "TIME SHIFTER"
                      Lines =
                        [ sprintf "Turns to rewind (1-10): %s" (if turnsText = "" then "_" else turnsText)
                          sprintf "Available: %d" (min 10 session.History.Length)
                          "Type digits, Enter confirms, Esc closes." ] } }
    | QuitConfirm ->
        Some
            { Cursor = None
              Panel =
                Some
                    { Style = CenterDialog
                      Title = "QUIT?"
                      Lines =
                        [ "Press Enter to quit."
                          "Press Esc to stay." ] } }
