(* SadConsole presentation layer: panel layout, camera control, input polling, and draggable splitters. *)
module Spelunk.Ui

open SadConsole
open SadConsole.Input
open SadRogue.Primitives
open Spelunk.Application
open Spelunk.Config
open Spelunk.Input
open Spelunk.Model
open Spelunk.Overlay
open Spelunk.Output
open Spelunk.Domain

let private ui = uiSettings ()
let private statsWidth = ui.StatsWidth
let private gapRows = ui.GapRows
let private logRows = ui.LogRows
let private cameraMargin = ui.CameraMargin
let private panelGap = ui.PanelGap

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

    // The camera only shifts once the focus point approaches the viewport margin.
    let next =
        if focus < minFocus then
            focus - cameraMargin
        elif focus > maxFocus then
            focus - viewportSize + cameraMargin + 1
        else
            current

    max 0 (min maxCamera next)

let private adjustCamera viewportWidth viewportHeight camera focus mapWidth mapHeight =
    { X = clampCameraAxis camera.X focus.X viewportWidth mapWidth
      Y = clampCameraAxis camera.Y focus.Y viewportHeight mapHeight }

let private toScreenPosition camera worldPosition =
    // Screen coordinates are world coordinates translated by the current camera origin.
    { X = worldPosition.X - camera.X
      Y = worldPosition.Y - camera.Y }

let private isVisible viewportWidth viewportHeight worldPosition camera =
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

type DividerPanel(width, height, glyph, onPress: MouseScreenObjectState -> unit, onDrag: MouseScreenObjectState -> unit, onRelease: unit -> unit) as this =
    inherit ScreenSurface(width, height)

    let mutable pressedAt: Point option = None

    do
        this.UseMouse <- true
        this.IsExclusiveMouse <- false
        this.FocusOnMouseClick <- false

    member this.Render() =
        clearSurface this

        for y in 0 .. this.Surface.Height - 1 do
            for x in 0 .. this.Surface.Width - 1 do
                paintCell this x y glyph Color.Gray Color.Black

        this.IsDirty <- true

    override _.ProcessMouse(state: MouseScreenObjectState) =
        if not state.IsOnScreenObject && not this.IsExclusiveMouse then
            base.ProcessMouse(state)
        elif state.Mouse.LeftButtonDown then
            match pressedAt with
            | None ->
                pressedAt <- Some state.WorldCellPosition
                this.IsExclusiveMouse <- true
                onPress state
            | Some start when start <> state.WorldCellPosition ->
                // A click without movement is ignored; resizing starts only after the drag crosses into a new cell.
                onDrag state
            | Some _ -> ()

            true
        else
            pressedAt <- None
            this.IsExclusiveMouse <- false
            onRelease ()
            base.ProcessMouse(state)

    override _.LostMouse(state: MouseScreenObjectState) =
        if not state.Mouse.LeftButtonDown then
            pressedAt <- None
            this.IsExclusiveMouse <- false
            onRelease ()

        base.LostMouse(state)

type StatsPanel(windowHeight) =
    inherit ScreenSurface(statsWidth, windowHeight)

    member this.Render(session, camera) =
        clearSurface this
        let width = this.Surface.Width
        let height = this.Surface.Height
        drawFrame this 0 0 width height
        writeText this 2 1 Color.Yellow "SCAV"
        writeText this 2 3 Color.White (sprintf "Depth %d" session.State.Depth)
        writeText this 2 5 Color.White (sprintf "HP %d/%d" session.State.Player.Hp session.State.Player.MaxHp)
        writeText this 2 7 Color.LightGray (sprintf "View %d,%d" camera.X camera.Y)
        writeText this 2 14 Color.LightGray "Look: X"
        writeText this 2 15 Color.LightGray "Target: F"
        writeText this 2 16 Color.LightGray "Inv: I"
        writeText this 2 17 Color.LightGray "Shift: T"
        writeText this 2 18 Color.LightGray "Wait: Sp/5"
        writeText this 2 19 Color.LightGray "Quit: Q"
        writeText this 2 20 Color.LightGray "Save: F5"
        writeText this 2 21 Color.LightGray "Load: F9"
        this.IsDirty <- true

type CavernPanel(viewportWidth, viewportHeight) =
    inherit ScreenSurface(viewportWidth, viewportHeight)

    member this.Render(session, camera) =
        clearSurface this
        let width = this.Surface.Width
        let height = this.Surface.Height

        for screenY in 0 .. height - 1 do
            for screenX in 0 .. width - 1 do
                let worldX = camera.X + screenX
                let worldY = camera.Y + screenY
                let tile = session.State.Map.Tiles[worldY, worldX]

                if session.State.VisibleTiles[worldY, worldX] then
                    paintCell this screenX screenY (tileGlyph tile) (tileForeground tile) Color.Black
                elif session.State.ExploredTiles[worldY, worldX] then
                    paintCell this screenX screenY (tileGlyph tile) (exploredTileForeground tile) Color.Black

        for monster in session.State.Monsters do
            if isVisible width height monster.Position camera
               && session.State.VisibleTiles[monster.Position.Y, monster.Position.X] then
                let screenPosition = toScreenPosition camera monster.Position
                paintCell this screenPosition.X screenPosition.Y monster.Glyph Color.IndianRed Color.Black

        if isVisible width height session.State.Player.Position camera then
            let screenPosition = toScreenPosition camera session.State.Player.Position
            paintCell this screenPosition.X screenPosition.Y session.State.Player.Glyph Color.Cyan Color.Black

        this.IsDirty <- true

type MessagesPanel(viewportWidth) =
    inherit ScreenSurface(viewportWidth, logRows)

    member this.Render(session) =
        clearSurface this
        let height = this.Surface.Height

        session.State.Messages
        |> List.truncate height
        |> List.rev
        |> List.iteri (fun i message -> writeText this 0 i Color.LightGray message)

        if session.State.Player.Hp <= 0 then
            writeText this 0 (height - 1) Color.OrangeRed "You died. Press Q to quit."

        this.IsDirty <- true

type OverlayPanelSurface(viewportWidth, viewportHeight) =
    inherit ScreenSurface(viewportWidth, viewportHeight)

    member this.Render(session, camera) =
        clearSurface this
        let width = this.Surface.Width
        let height = this.Surface.Height

        match overlayViewModel session with
        | None ->
            this.IsVisible <- false
        | Some overlay ->
            this.IsVisible <- true

            match overlay.Cursor with
            | Some cursor ->
                if isVisible width height cursor.Position camera then
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
                let left, top, panelWidth, panelHeight = panelLayout width height panel.Style panel.Lines.Length
                drawFrame this left top panelWidth panelHeight
                writeText this (left + 2) (top + 1) Color.Yellow panel.Title

                panel.Lines
                |> List.truncate (max 0 (panelHeight - 3))
                |> List.iteri (fun i line -> writeText this (left + 2) (top + 2 + i) Color.LightGray line)
            | None -> ()

        this.IsDirty <- true

type RootScreen(screenWidth, screenHeight) as this =
    inherit ScreenObject()

    let dividerThickness = 1
    let minStatsWidth = 12
    let minMessagesHeight = 3
    let minCavernWidth = 20
    let minCavernHeight = 8
    let mutable statsPanelWidth = max minStatsWidth statsWidth
    let mutable messagesPanelHeight = max minMessagesHeight logRows
    let mutable session = initialSession ()
    let mutable camera = { X = 0; Y = 0 }
    let bindings = defaultBindings
    let mutable relayout = (fun () -> ())
    let mutable requestRedraw = (fun () -> ())
    let mutable activeDrag: string option = None
    let statsPanel = new StatsPanel(screenHeight)
    let verticalDivider =
        new DividerPanel(
            dividerThickness,
            screenHeight,
            '|',
            (fun _ ->
                match activeDrag with
                | None -> activeDrag <- Some "vertical"
                | _ -> ()),
            (fun state ->
                match activeDrag with
                | Some "vertical" ->
                    statsPanelWidth <- state.WorldCellPosition.X
                    relayout ()
                    requestRedraw ()
                | _ -> ()),
            (fun () ->
                match activeDrag with
                | Some "vertical" -> activeDrag <- None
                | _ -> ()))
    let cavernPanel = new CavernPanel(max 1 (screenWidth - statsPanelWidth - dividerThickness), max 1 (screenHeight - dividerThickness - messagesPanelHeight))
    let horizontalDivider =
        new DividerPanel(
            cavernPanel.Surface.Width,
            dividerThickness,
            '-',
            (fun _ ->
                match activeDrag with
                | None -> activeDrag <- Some "horizontal"
                | _ -> ()),
            (fun state ->
                match activeDrag with
                | Some "horizontal" ->
                    messagesPanelHeight <- screenHeight - state.WorldCellPosition.Y - dividerThickness
                    relayout ()
                    requestRedraw ()
                | _ -> ()),
            (fun () ->
                match activeDrag with
                | Some "horizontal" -> activeDrag <- None
                | _ -> ()))
    let messagesPanel = new MessagesPanel(cavernPanel.Surface.Width)
    let overlayPanel = new OverlayPanelSurface(cavernPanel.Surface.Width, cavernPanel.Surface.Height)

    let configureSurface (surface: ScreenSurface) x y =
        surface.Position <- Point(x, y)
        this.Children.Add(surface) |> ignore

    let layoutPanels () =
        statsPanelWidth <- max minStatsWidth (min (screenWidth - dividerThickness - minCavernWidth) statsPanelWidth)
        let rightWidth = max minCavernWidth (screenWidth - statsPanelWidth - dividerThickness)
        messagesPanelHeight <- max minMessagesHeight (min (screenHeight - dividerThickness - minCavernHeight) messagesPanelHeight)
        let cavernHeight = max minCavernHeight (screenHeight - dividerThickness - messagesPanelHeight)
        let mapLeft = statsPanelWidth + dividerThickness
        let dividerTop = cavernHeight

        // The cavern expands to consume whatever space remains after sidebar and log constraints are applied.
        statsPanel.Resize(statsPanelWidth, screenHeight, false)
        statsPanel.Position <- Point(0, 0)

        verticalDivider.Resize(dividerThickness, screenHeight, false)
        verticalDivider.Position <- Point(statsPanelWidth, 0)

        cavernPanel.Resize(rightWidth, cavernHeight, false)
        cavernPanel.Position <- Point(mapLeft, 0)

        horizontalDivider.Resize(rightWidth, dividerThickness, false)
        horizontalDivider.Position <- Point(mapLeft, dividerTop)

        messagesPanel.Resize(rightWidth, messagesPanelHeight, false)
        messagesPanel.Position <- Point(mapLeft, dividerTop + dividerThickness)

        overlayPanel.Resize(rightWidth, cavernHeight, false)
        overlayPanel.Position <- Point(mapLeft, 0)

    let redraw () =
        let viewportWidth = cavernPanel.Surface.Width
        let viewportHeight = cavernPanel.Surface.Height
        let cameraFocus =
            match session.Modal with
            | LookMode cursor
            | TargetMode cursor -> cursor
            | _ -> session.State.Player.Position

        // Modal cursors reuse the same camera-follow rules as the player.
        camera <- adjustCamera viewportWidth viewportHeight camera cameraFocus session.State.Map.Width session.State.Map.Height
        statsPanel.Render(session, camera)
        verticalDivider.Render()
        cavernPanel.Render(session, camera)
        horizontalDivider.Render()
        messagesPanel.Render(session)
        overlayPanel.Render(session, camera)

    do
        relayout <- layoutPanels
        requestRedraw <- redraw

    let applyIntentAndRedraw intent =
        let transition = applyIntent intent session

        match transition.NextSession with
        | Some nextSession ->
            session <- nextSession
        | None ->
            Game.Instance.MonoGameInstance.Exit()

        redraw ()

    let tryGetKey session (keyboard: Keyboard) =
        let digitKey =
            match session.Modal with
            | TimeShiftPrompt _ ->
                if keyboard.IsKeyPressed(Keys.Back) then
                    Some BackspaceKey
                elif keyboard.IsKeyPressed(Keys.D0) || keyboard.IsKeyPressed(Keys.NumPad0) then
                    Some(DigitKey 0)
                elif keyboard.IsKeyPressed(Keys.D1) || keyboard.IsKeyPressed(Keys.NumPad1) then
                    Some(DigitKey 1)
                elif keyboard.IsKeyPressed(Keys.D2) || keyboard.IsKeyPressed(Keys.NumPad2) then
                    Some(DigitKey 2)
                elif keyboard.IsKeyPressed(Keys.D3) || keyboard.IsKeyPressed(Keys.NumPad3) then
                    Some(DigitKey 3)
                elif keyboard.IsKeyPressed(Keys.D4) || keyboard.IsKeyPressed(Keys.NumPad4) then
                    Some(DigitKey 4)
                elif keyboard.IsKeyPressed(Keys.D5) || keyboard.IsKeyPressed(Keys.NumPad5) then
                    Some(DigitKey 5)
                elif keyboard.IsKeyPressed(Keys.D6) || keyboard.IsKeyPressed(Keys.NumPad6) then
                    Some(DigitKey 6)
                elif keyboard.IsKeyPressed(Keys.D7) || keyboard.IsKeyPressed(Keys.NumPad7) then
                    Some(DigitKey 7)
                elif keyboard.IsKeyPressed(Keys.D8) || keyboard.IsKeyPressed(Keys.NumPad8) then
                    Some(DigitKey 8)
                elif keyboard.IsKeyPressed(Keys.D9) || keyboard.IsKeyPressed(Keys.NumPad9) then
                    Some(DigitKey 9)
                else
                    None
            | _ -> None

        match digitKey with
        | Some key -> Some key
        | None ->
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
            elif keyboard.IsKeyPressed(Keys.T) then
                Some TimeShifterKey
            elif keyboard.IsKeyPressed(Keys.F5) then
                Some SaveKey
            elif keyboard.IsKeyPressed(Keys.F9) then
                Some LoadKey
            elif keyboard.IsKeyPressed(Keys.NumPad7) then
                Some UpLeft
            elif keyboard.IsKeyPressed(Keys.NumPad8) then
                Some Up
            elif keyboard.IsKeyPressed(Keys.NumPad9) then
                Some UpRight
            elif keyboard.IsKeyPressed(Keys.NumPad4) then
                Some Left
            elif keyboard.IsKeyPressed(Keys.NumPad6) then
                Some Right
            elif keyboard.IsKeyPressed(Keys.NumPad1) then
                Some DownLeft
            elif keyboard.IsKeyPressed(Keys.NumPad2) then
                Some Down
            elif keyboard.IsKeyPressed(Keys.NumPad3) then
                Some DownRight
            elif keyboard.IsKeyPressed(Keys.NumPad5) then
                Some WaitKey
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
        configureSurface verticalDivider 0 0
        configureSurface cavernPanel 0 0
        configureSurface horizontalDivider 0 0
        configureSurface messagesPanel 0 0
        configureSurface overlayPanel 0 0
        overlayPanel.IsVisible <- false
        layoutPanels ()
        redraw ()

    override _.ProcessKeyboard(keyboard: Keyboard) =
        match tryGetKey session keyboard with
        | Some key ->
            key |> intentFromKey bindings |> normalizeIntent session |> applyIntentAndRedraw
            true
        | None -> base.ProcessKeyboard(keyboard)
