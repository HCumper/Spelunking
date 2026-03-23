module Spelunk.Ui

open SadConsole
open SadConsole.Input
open SadRogue.Primitives
open Spelunk.Application
open Spelunk.Domain

let private statsRows = 2
let private gapRows = 1
let private logRows = 6
let private viewportWidth = 40
let private viewportHeight = 20
let private cameraMargin = 8
let private mapTop = statsRows + gapRows
let private logTop mapHeight = mapTop + mapHeight + gapRows
let private totalHeight mapHeight = mapHeight + statsRows + gapRows + logRows

let private tileGlyph tile =
    match tile with
    | Wall -> '#'
    | Floor -> '.'
    | StairsDown -> '>'

let private tileForeground tile =
    match tile with
    | Wall -> Color.Gray
    | Floor -> Color.DarkSlateGray
    | StairsDown -> Color.Gold

let private exploredTileForeground tile =
    match tile with
    | Wall -> Color.DarkGray
    | Floor -> Color.DimGray
    | StairsDown -> Color.DarkGoldenrod

let private overlayColor color =
    match color with
    | OverlayColor.Default -> Color.White
    | OverlayColor.Inverted -> Color.Black
    | OverlayColor.Highlight -> Color.Orange

let private overlayCursorGlyph style =
    match style with
    | OverlayCursorStyle.InspectCursor -> 'X'
    | OverlayCursorStyle.TargetCursor -> '*'

let private panelLayout width mapHeight style lineCount =
    match style with
    | OverlayPanelStyle.FullWidthFooter ->
        0, mapHeight - 5, width, 4
    | OverlayPanelStyle.CenterDialog ->
        let boxWidth = width - 12
        let contentHeight = max 4 (lineCount + 3)
        6, 3, boxWidth, contentHeight

let private clampCameraAxis current focus viewportSize worldSize =
    let minFocus = current + cameraMargin
    let maxFocus = current + viewportSize - cameraMargin - 1
    let maxCamera = max 0 (worldSize - viewportSize)

    let next =
        if focus < minFocus then
            focus - cameraMargin
        elif focus > maxFocus then
            focus - viewportSize + cameraMargin + 1
        else
            current

    max 0 (min maxCamera next)

let private adjustCamera camera focus mapWidth mapHeight =
    { X = clampCameraAxis camera.X focus.X viewportWidth mapWidth
      Y = clampCameraAxis camera.Y focus.Y viewportHeight mapHeight }

let private toScreenPosition camera worldPosition =
    { X = worldPosition.X - camera.X
      Y = worldPosition.Y - camera.Y }

let private isVisible worldPosition camera =
    worldPosition.X >= camera.X
    && worldPosition.X < camera.X + viewportWidth
    && worldPosition.Y >= camera.Y
    && worldPosition.Y < camera.Y + viewportHeight

let private paintCell (surface: ScreenSurface) x y glyph foreground background =
    surface.SetGlyph(x, y, int glyph)
    surface.SetForeground(x, y, foreground)
    surface.SetBackground(x, y, background)

let private clearSurface (surface: ScreenSurface) =
    for y in 0 .. surface.Surface.Height - 1 do
        for x in 0 .. surface.Surface.Width - 1 do
            paintCell surface x y ' ' Color.White Color.Black

let private writeText (surface: ScreenSurface) x y foreground text =
    text
    |> Seq.truncate (max 0 (surface.Surface.Width - x))
    |> Seq.iteri (fun i ch -> paintCell surface (x + i) y ch foreground Color.Black)

let private drawFrame (surface: ScreenSurface) left top frameWidth frameHeight =
    for x in left .. left + frameWidth - 1 do
        paintCell surface x top '-' Color.White Color.Black
        paintCell surface x (top + frameHeight - 1) '-' Color.White Color.Black

    for y in top .. top + frameHeight - 1 do
        paintCell surface left y '|' Color.White Color.Black
        paintCell surface (left + frameWidth - 1) y '|' Color.White Color.Black

    paintCell surface left top '+' Color.White Color.Black
    paintCell surface (left + frameWidth - 1) top '+' Color.White Color.Black
    paintCell surface left (top + frameHeight - 1) '+' Color.White Color.Black
    paintCell surface (left + frameWidth - 1) (top + frameHeight - 1) '+' Color.White Color.Black

type StatsPanel() =
    inherit ScreenSurface(viewportWidth, statsRows)

    member this.Render(session, camera) =
        clearSurface this
        writeText this 0 0 Color.White (sprintf "Depth %d" session.State.Depth)
        writeText this 12 0 Color.White (sprintf "HP %d/%d" session.State.Player.Hp session.State.Player.MaxHp)
        writeText this 0 1 Color.LightGray (sprintf "View %d,%d" camera.X camera.Y)
        this.IsDirty <- true

type CavernPanel() =
    inherit ScreenSurface(viewportWidth, viewportHeight)

    member this.Render(session, camera) =
        clearSurface this

        for screenY in 0 .. viewportHeight - 1 do
            for screenX in 0 .. viewportWidth - 1 do
                let worldX = camera.X + screenX
                let worldY = camera.Y + screenY
                let tile = session.State.Map.Tiles[worldY, worldX]

                if session.State.VisibleTiles[worldY, worldX] then
                    paintCell this screenX screenY (tileGlyph tile) (tileForeground tile) Color.Black
                elif session.State.ExploredTiles[worldY, worldX] then
                    paintCell this screenX screenY (tileGlyph tile) (exploredTileForeground tile) Color.Black

        for monster in session.State.Monsters do
            if isVisible monster.Position camera && session.State.VisibleTiles[monster.Position.Y, monster.Position.X] then
                let screenPosition = toScreenPosition camera monster.Position
                paintCell this screenPosition.X screenPosition.Y monster.Glyph Color.IndianRed Color.Black

        if isVisible session.State.Player.Position camera then
            let screenPosition = toScreenPosition camera session.State.Player.Position
            paintCell this screenPosition.X screenPosition.Y session.State.Player.Glyph Color.Cyan Color.Black

        this.IsDirty <- true

type MessagesPanel() =
    inherit ScreenSurface(viewportWidth, logRows)

    member this.Render(session) =
        clearSurface this

        session.State.Messages
        |> List.truncate logRows
        |> List.rev
        |> List.iteri (fun i message -> writeText this 0 i Color.LightGray message)

        if session.State.Player.Hp <= 0 then
            writeText this 0 (logRows - 1) Color.OrangeRed "You died. Press Q to quit."

        this.IsDirty <- true

type OverlayPanelSurface() =
    inherit ScreenSurface(viewportWidth, viewportHeight)

    member this.Render(session, camera) =
        clearSurface this

        match overlayViewModel session with
        | None ->
            this.IsVisible <- false
        | Some overlay ->
            this.IsVisible <- true

            match overlay.Cursor with
            | Some cursor ->
                if isVisible cursor.Position camera then
                    let screenPosition = toScreenPosition camera cursor.Position

                    paintCell
                        this
                        screenPosition.X
                        screenPosition.Y
                        (overlayCursorGlyph cursor.Style)
                        (overlayColor cursor.Foreground)
                        (overlayColor cursor.Background)
            | None -> ()

            match overlay.Panel with
            | Some panel ->
                let left, top, panelWidth, panelHeight = panelLayout viewportWidth viewportHeight panel.Style panel.Lines.Length
                drawFrame this left top panelWidth panelHeight
                writeText this (left + 2) (top + 1) Color.Yellow panel.Title

                panel.Lines
                |> List.truncate (max 0 (panelHeight - 3))
                |> List.iteri (fun i line -> writeText this (left + 2) (top + 2 + i) Color.LightGray line)
            | None -> ()

        this.IsDirty <- true

type RootScreen() as this =
    inherit ScreenObject()

    let mutable session = initialSession ()
    let mutable camera = { X = 0; Y = 0 }
    let bindings = defaultBindings
    let statsPanel = new StatsPanel()
    let cavernPanel = new CavernPanel()
    let messagesPanel = new MessagesPanel()
    let overlayPanel = new OverlayPanelSurface()

    let configureSurface (surface: ScreenSurface) x y =
        surface.Position <- Point(x, y)
        this.Children.Add(surface) |> ignore

    let redraw () =
        let cameraFocus =
            match session.Modal with
            | LookMode cursor
            | TargetMode cursor -> cursor
            | _ -> session.State.Player.Position

        camera <- adjustCamera camera cameraFocus session.State.Map.Width session.State.Map.Height
        statsPanel.Render(session, camera)
        cavernPanel.Render(session, camera)
        messagesPanel.Render(session)
        overlayPanel.Render(session, camera)

    let applyIntentAndRedraw intent =
        match applyIntent intent session with
        | Some nextSession ->
            session <- nextSession
        | None ->
            Game.Instance.MonoGameInstance.Exit()

        redraw ()

    let tryGetKey (keyboard: Keyboard) =
        if keyboard.IsKeyPressed(Keys.Q) then
            Some QuitKey
        elif keyboard.IsKeyPressed(Keys.Escape) then
            Some CancelKey
        elif keyboard.IsKeyPressed(Keys.Enter) then
            Some ConfirmKey
        elif keyboard.IsKeyPressed(Keys.X) then
            Some LookKey
        elif keyboard.IsKeyPressed(Keys.F) then
            Some TargetKey
        elif keyboard.IsKeyPressed(Keys.I) then
            Some InventoryKey
        elif keyboard.IsKeyPressed(Keys.Up) || keyboard.IsKeyPressed(Keys.W) then
            Some Up
        elif keyboard.IsKeyPressed(Keys.Down) || keyboard.IsKeyPressed(Keys.S) then
            Some Down
        elif keyboard.IsKeyPressed(Keys.Left) || keyboard.IsKeyPressed(Keys.A) then
            Some Left
        elif keyboard.IsKeyPressed(Keys.Right) || keyboard.IsKeyPressed(Keys.D) then
            Some Right
        elif keyboard.IsKeyPressed(Keys.Space) then
            Some WaitKey
        else
            None

    do
        this.UseKeyboard <- true
        configureSurface statsPanel 0 0
        configureSurface cavernPanel 0 mapTop
        configureSurface messagesPanel 0 (logTop viewportHeight)
        configureSurface overlayPanel 0 mapTop
        overlayPanel.IsVisible <- false
        redraw ()

    override _.ProcessKeyboard(keyboard: Keyboard) =
        match tryGetKey keyboard with
        | Some key ->
            key |> intentFromKey bindings |> normalizeIntent session |> applyIntentAndRedraw
            true
        | None -> base.ProcessKeyboard(keyboard)
