(* Bolero rendering for the browser host. *)
module Spelunking.Web.Render

open Bolero.Html
open Spelunk.Application
open Spelunk.Model
open Spelunk.Overlay
open Spelunking.Web.App

let private viewportWidth = 72
let private viewportHeight = 30

let private glyphFromCode glyph =
    if glyph >= 32 && glyph <= 126 then
        string (char glyph)
    else
        "?"

let private tileGlyph tile =
    match tile with
    | Wall -> "#"
    | Floor -> "."
    | Tardis -> "T"

let private tileClass tile explored =
    match tile, explored with
    | Wall, false -> "tile wall"
    | Wall, true -> "tile wall explored"
    | Floor, false -> "tile floor"
    | Floor, true -> "tile floor explored"
    | Tardis, false -> "tile tardis"
    | Tardis, true -> "tile tardis explored"

let private monsterClass monster =
    match glyphFromCode monster.Glyph with
    | "D" -> "tile monster dalek"
    | "C" -> "tile monster cyberman"
    | "W" -> "tile monster angel"
    | _ -> "tile monster"

let private overlayCursorAt position overlay =
    overlay
    |> Option.bind (fun view -> view.Cursor)
    |> Option.filter (fun cursor -> cursor.Position = position)

let private monsterAt position state =
    state.Monsters
    |> List.tryFind (fun monster -> monster.Position = position)

let private cellView session overlay camera screenX screenY =
    let worldX = camera.X + screenX
    let worldY = camera.Y + screenY
    let position = { X = worldX; Y = worldY }
    let state = session.State

    if worldX >= state.Map.Width || worldY >= state.Map.Height then
        span { attr.``class`` "tile hidden"; " " }
    else
        let cursor = overlayCursorAt position overlay

        match cursor with
        | Some cursor ->
            let marker =
                match cursor.Style with
                | InspectCursor -> "X"
                | TargetCursor -> "*"

            span { attr.``class`` "tile cursor"; marker }
        | None when position = state.Player.Position ->
            span { attr.``class`` "tile player"; glyphFromCode state.Player.Glyph }
        | None ->
            match monsterAt position state with
            | Some monster when state.VisibleTiles[worldY, worldX] ->
                span { attr.``class`` (monsterClass monster); glyphFromCode monster.Glyph }
            | _ when state.VisibleTiles[worldY, worldX] ->
                let tile = state.Map.Tiles[worldY, worldX]
                span { attr.``class`` (tileClass tile false); tileGlyph tile }
            | _ when state.ExploredTiles[worldY, worldX] ->
                let tile = state.Map.Tiles[worldY, worldX]
                span { attr.``class`` (tileClass tile true); tileGlyph tile }
            | _ ->
                span { attr.``class`` "tile hidden"; " " }

let private modalName modal =
    match modal with
    | NoModal -> "Explore"
    | LookMode _ -> "Look"
    | TargetMode _ -> "Target"
    | InventoryMode -> "Inventory"
    | TimeShiftPrompt _ -> "Time"
    | QuitConfirm -> "Quit?"

let private overlayPanelView overlay =
    match overlay |> Option.bind (fun view -> view.Panel) with
    | None -> empty()
    | Some panel ->
        div {
            attr.``class``
                (match panel.Style with
                 | FullWidthFooter -> "overlay-panel footer"
                 | CenterDialog -> "overlay-panel dialog")

            h2 { panel.Title }

            for line in panel.Lines do
                p { line }
        }

let view model dispatch =
    let session = model.Session
    let state = session.State
    let camera = model.Camera
    let overlay = overlayViewModel session

    div {
        attr.``class`` "spelunk-shell"
        attr.tabindex "0"

        aside {
            attr.``class`` "stats-panel"

            h1 { "SCAV" }

            dl {
                dt { "World" }
                dd { string state.World }
                dt { "HP" }
                dd { $"{state.Player.Hp}/{state.Player.MaxHp}" }
                dt { "Turn" }
                dd { string state.TurnCount }
                dt { "View" }
                dd { $"{camera.X},{camera.Y}" }
                dt { "Mode" }
                dd { modalName session.Modal }
                dt { "Weapon" }
                dd { state.Player.RangedWeapon.Name }
                dt { "Ammo" }
                dd {
                    match state.Player.RangedWeapon.Ammo with
                    | Some ammo -> string ammo
                    | None -> "-"
                }
            }

            nav {
                attr.``class`` "command-list"
                span { "Look: L" }
                span { "Target: F" }
                span { "Inv: I" }
                span { "Shift: T" }
                span { "Wait: Space" }
                span { "Save: F5" }
                span { "Load: F9" }
            }
        }

        div {
            attr.``class`` "play-area"

            div {
                attr.``class`` "map-wrap"

                div {
                    attr.``class`` "game-map"

                    for y in 0 .. viewportHeight - 1 do
                        div {
                            attr.``class`` "map-row"

                            for x in 0 .. viewportWidth - 1 do
                                cellView session overlay camera x y
                        }
                }

                overlayPanelView overlay
            }

            div {
                attr.``class`` "message-log"

                for message in state.Messages |> List.truncate 4 |> List.rev do
                    p { message }

                if state.Player.Hp <= 0 then
                    p {
                        attr.``class`` "danger"
                        "You died. Press Q to quit."
                    }
            }
        }
    }
