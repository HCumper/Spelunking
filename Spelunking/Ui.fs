(* SadConsole presentation layer: panel layout, camera control, input polling, and draggable splitters. *)
module Spelunk.Ui

open System
open SadConsole
open SadConsole.Input
open SadRogue.Primitives
open Spelunk.Appearance
open Spelunk.Application
open Spelunk.Config
open Spelunk.Input
open Spelunk.Model
open Spelunk.Overlay
open Spelunk.Output
open Spelunk.Domain
open Spelunk.SessionActions
open Spelunk.Services

let private ui = uiSettings ()
let private statsWidth = ui.StatsWidth
let private gapRows = ui.GapRows
let private logRows = ui.LogRows
let private cameraMargin = ui.CameraMargin
let private panelGap = ui.PanelGap

let private overlayColor color =
    match color with
    | OverlayColor.Default -> Color.White
    | OverlayColor.Inverted -> Color.Black
    | OverlayColor.Highlight -> Color.Orange

let private overlayCursorGlyph style =
    match style with
    | OverlayCursorStyle.InspectCursor -> int 'X'
    | OverlayCursorStyle.TargetCursor -> int '*'

let private panelLayout width mapHeight style lineCount =
    match style with
    | OverlayPanelStyle.FullWidthFooter ->
        0, mapHeight - 5, width, 4
    | OverlayPanelStyle.CenterDialog ->
        let boxWidth = width - 12
        let contentHeight = max 4 (lineCount + 3)
        6, 3, boxWidth, contentHeight

let private clampCameraAxis current focus viewportSize worldSize =
    let effectiveMargin =
        cameraMargin
        |> min (max 0 ((viewportSize - 1) / 2))

    let minFocus = current + effectiveMargin
    let maxFocus = current + viewportSize - effectiveMargin - 1
    let maxCamera = max 0 (worldSize - viewportSize)

    let next =
        if focus < minFocus then
            focus - effectiveMargin
        elif focus > maxFocus then
            focus - viewportSize + effectiveMargin + 1
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

let private regenerationFrames oldGlyph newGlyph =
    [ { Glyph = oldGlyph; Foreground = Color.White; Background = Color.Black }
      { Glyph = oldGlyph; Foreground = Color.Gold; Background = Color.Black }
      { Glyph = newGlyph; Foreground = Color.White; Background = Color.DarkGoldenrod }
      { Glyph = oldGlyph; Foreground = Color.LightYellow; Background = Color.DarkSlateBlue }
      { Glyph = newGlyph; Foreground = Color.LightCyan; Background = Color.DarkBlue }
      { Glyph = newGlyph; Foreground = Color.White; Background = Color.Black } ]

let private paintCell (surface: ScreenSurface) (x: int) (y: int) (glyph: int) foreground background =
    surface.SetGlyph(x, y, glyph)
    surface.SetForeground(x, y, foreground)
    surface.SetBackground(x, y, background)

let private paintAppearance surface x y (appearance: CellAppearance) =
    paintCell surface x y appearance.Glyph appearance.Foreground appearance.Background

let private clearSurface (surface: ScreenSurface) =
    for y in 0 .. surface.Surface.Height - 1 do
        for x in 0 .. surface.Surface.Width - 1 do
            paintCell surface x y (int ' ') Color.White Color.Black

let private clearOverlaySurface (surface: ScreenSurface) =
    for y in 0 .. surface.Surface.Height - 1 do
        for x in 0 .. surface.Surface.Width - 1 do
            paintCell surface x y (int ' ') Color.Transparent Color.Transparent

let private writeText (surface: ScreenSurface) x y foreground text =
    text
    |> Seq.truncate (max 0 (surface.Surface.Width - x))
    |> Seq.iteri (fun i ch -> paintCell surface (x + i) y (int ch) foreground Color.Black)

let private drawFrame (surface: ScreenSurface) left top frameWidth frameHeight =
    for x in left .. left + frameWidth - 1 do
        paintCell surface x top (int '-') Color.White Color.Black
        paintCell surface x (top + frameHeight - 1) (int '-') Color.White Color.Black

    for y in top .. top + frameHeight - 1 do
        paintCell surface left y (int '|') Color.White Color.Black
        paintCell surface (left + frameWidth - 1) y (int '|') Color.White Color.Black

    paintCell surface left top (int '+') Color.White Color.Black
    paintCell surface (left + frameWidth - 1) top (int '+') Color.White Color.Black
    paintCell surface left (top + frameHeight - 1) (int '+') Color.White Color.Black
    paintCell surface (left + frameWidth - 1) (top + frameHeight - 1) (int '+') Color.White Color.Black

let private keyTable : (Key * Keys list) list =
    [ QuitKey, [ Keys.Q ]
      CancelKey, [ Keys.Escape ]
      ConfirmKey, [ Keys.Enter ]
      LookKey, [ Keys.L ]
      TargetKey, [ Keys.F ]
      InventoryKey, [ Keys.I ]
      TimeShifterKey, [ Keys.T ]
      SaveKey, [ Keys.F5 ]
      LoadKey, [ Keys.F9 ]
      UpLeft, [ Keys.NumPad7 ]
      Up, [ Keys.NumPad8; Keys.Up; Keys.W ]
      UpRight, [ Keys.NumPad9 ]
      Left, [ Keys.NumPad4; Keys.Left; Keys.A ]
      Right, [ Keys.NumPad6; Keys.Right; Keys.D ]
      DownLeft, [ Keys.NumPad1 ]
      Down, [ Keys.NumPad2; Keys.Down; Keys.S ]
      DownRight, [ Keys.NumPad3 ]
      WaitKey, [ Keys.NumPad5; Keys.Space ] ]

let private timeShiftPromptKeyTable : (Key * Keys list) list =
    [ BackspaceKey, [ Keys.Back ]
      DigitKey 0, [ Keys.D0; Keys.NumPad0 ]
      DigitKey 1, [ Keys.D1; Keys.NumPad1 ]
      DigitKey 2, [ Keys.D2; Keys.NumPad2 ]
      DigitKey 3, [ Keys.D3; Keys.NumPad3 ]
      DigitKey 4, [ Keys.D4; Keys.NumPad4 ]
      DigitKey 5, [ Keys.D5; Keys.NumPad5 ]
      DigitKey 6, [ Keys.D6; Keys.NumPad6 ]
      DigitKey 7, [ Keys.D7; Keys.NumPad7 ]
      DigitKey 8, [ Keys.D8; Keys.NumPad8 ]
      DigitKey 9, [ Keys.D9; Keys.NumPad9 ] ]

let private pressedMappedKey (keyboard: Keyboard) (mappings: (Key * Keys list) list) =
    mappings
    |> List.tryFind (fun (_, keys) -> keys |> List.exists keyboard.IsKeyPressed)
    |> Option.map fst

let private runDirectionMappings : ((int * int) * Keys list) list =
    [ (-1, -1), [ Keys.NumPad7 ]
      (0, -1), [ Keys.NumPad8 ]
      (1, -1), [ Keys.NumPad9 ]
      (-1, 0), [ Keys.NumPad4 ]
      (1, 0), [ Keys.NumPad6 ]
      (-1, 1), [ Keys.NumPad1 ]
      (0, 1), [ Keys.NumPad2 ]
      (1, 1), [ Keys.NumPad3 ] ]

let private pressedRunIntent (keyboard: Keyboard) =
    let runModifierHeld =
        keyboard.IsKeyDown(Keys.OemPeriod)
        || keyboard.IsKeyDown(Keys.Decimal)

    if not runModifierHeld then
        None
    else
        runDirectionMappings
        |> List.tryFind (fun (_, keys) -> keys |> List.exists keyboard.IsKeyPressed)
        |> Option.map (fun ((dx, dy), _) -> Run(dx, dy))

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

    new(windowHeight, font: IFont, fontSize: Point) =
        new StatsPanel(windowHeight)
        then
            base.Font <- font
            base.FontSize <- fontSize

    member this.Render(session: Session, camera) =
        clearSurface this
        let width = this.Surface.Width
        let height = this.Surface.Height
        drawFrame this 0 0 width height
        writeText this 2 1 Color.Yellow "SCAV"
        writeText this 2 3 Color.White (sprintf "World %d" session.State.World)
        writeText this 2 5 Color.White (sprintf "HP %d/%d" session.State.Player.Hp session.State.Player.MaxHp)
        writeText this 2 7 Color.LightGray (sprintf "View %d,%d" camera.X camera.Y)
        writeText this 2 14 Color.LightGray "Look: L"
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

    new(viewportWidth, viewportHeight, font: IFont, fontSize: Point) =
        new CavernPanel(viewportWidth, viewportHeight)
        then
            base.Font <- font
            base.FontSize <- fontSize

    member this.Render(session: Session, camera) =
        clearSurface this
        let width = this.Surface.Width
        let height = this.Surface.Height

        for screenY in 0 .. height - 1 do
            for screenX in 0 .. width - 1 do
                let worldX = camera.X + screenX
                let worldY = camera.Y + screenY
                let tile = session.State.Map.Tiles[worldY, worldX]

                if session.State.VisibleTiles[worldY, worldX] then
                    paintAppearance this screenX screenY (tileAppearance tile false)
                elif session.State.ExploredTiles[worldY, worldX] then
                    paintAppearance this screenX screenY (tileAppearance tile true)

        for monster in session.State.Monsters do
            if isVisible width height monster.Position camera
               && session.State.VisibleTiles[monster.Position.Y, monster.Position.X] then
                let screenPosition = toScreenPosition camera monster.Position
                paintAppearance this screenPosition.X screenPosition.Y (monsterAppearance monster)

        if isVisible width height session.State.Player.Position camera then
            let screenPosition = toScreenPosition camera session.State.Player.Position
            paintAppearance this screenPosition.X screenPosition.Y (playerAppearance session.State.Player)

        this.IsDirty <- true

type MessagesPanel(viewportWidth) =
    inherit ScreenSurface(viewportWidth, logRows)

    new(viewportWidth, font: IFont, fontSize: Point) =
        new MessagesPanel(viewportWidth)
        then
            base.Font <- font
            base.FontSize <- fontSize

    member this.Render(session: Session) =
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

    new(viewportWidth, viewportHeight, font: IFont, fontSize: Point) =
        new OverlayPanelSurface(viewportWidth, viewportHeight)
        then
            base.Font <- font
            base.FontSize <- fontSize

    member this.Render(session: Session, camera) =
        clearOverlaySurface this
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

type RootScreen(screenWidth, screenHeight, tileFont: (IFont * IFont.Sizes) option, textFont: IFont, textFontSize: IFont.Sizes) as this =
    inherit ScreenObject()

    let dividerThickness = 1
    let minStatsWidth = 12
    let minMessagesHeight = 3
    let minCavernWidth = 20
    let minCavernHeight = 8
    let panelGapWidth = max 0 panelGap
    let mutable statsPanelWidth = max minStatsWidth statsWidth
    let mutable messagesPanelHeight = max minMessagesHeight logRows
    let mutable session = initialSession ()
    let mutable camera = { X = 0; Y = 0 }
    let bindings = defaultBindings
    let services = liveServices
    let mutable relayout = (fun () -> ())
    let mutable requestRedraw = (fun () -> ())
    let mutable activeDrag: string option = None
    let mutable spokenMonsterIds: Set<int> = Set.empty
    let mutable projectileAnimation: (Position list * int * TimeSpan) option = None
    let mutable regenerationAnimation: (Position * CellAppearance list * int * TimeSpan) option = None
    let projectileFrameDuration = TimeSpan.FromMilliseconds 35.0
    let regenerationFrameDuration = TimeSpan.FromMilliseconds 70.0
    let textCellSize = textFont.GetFontSize(textFontSize)
    let tileCellSize =
        match tileFont with
        | Some (font, fontSize) -> font.GetFontSize(fontSize)
        | None -> textCellSize
    let toTileCellsX textCells = max 1 ((textCells * textCellSize.X) / max 1 tileCellSize.X)
    let toTileCellsY textCells = max 1 ((textCells * textCellSize.Y) / max 1 tileCellSize.Y)
    let statsPanel = new StatsPanel(screenHeight, textFont, textCellSize)
    let verticalDivider =
        new DividerPanel(
            dividerThickness,
            screenHeight,
            int '|',
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
    let cavernPanel =
        let width = max 1 (toTileCellsX (screenWidth - statsPanelWidth - dividerThickness - panelGapWidth))
        let height = max 1 (toTileCellsY (screenHeight - dividerThickness - messagesPanelHeight))

        match tileFont with
        | Some (font, fontSize) -> new CavernPanel(width, height, font, font.GetFontSize(fontSize))
        | None -> new CavernPanel(width, height)
    let horizontalDivider =
        new DividerPanel(
            cavernPanel.Surface.Width,
            dividerThickness,
            int '-',
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
    let messagesPanel = new MessagesPanel(cavernPanel.Surface.Width, textFont, textCellSize)
    let overlayPanel =
        match tileFont with
        | Some (font, fontSize) -> new OverlayPanelSurface(cavernPanel.Surface.Width, cavernPanel.Surface.Height, font, font.GetFontSize(fontSize))
        | None -> new OverlayPanelSurface(cavernPanel.Surface.Width, cavernPanel.Surface.Height, textFont, textCellSize)

    let configureSurface (surface: ScreenSurface) x y =
        surface.Position <- Point(x, y)
        this.Children.Add(surface) |> ignore

    let layoutPanels () =
        statsPanelWidth <- max minStatsWidth (min (screenWidth - dividerThickness - panelGapWidth - minCavernWidth) statsPanelWidth)
        let rightWidth = max minCavernWidth (screenWidth - statsPanelWidth - dividerThickness - panelGapWidth)
        messagesPanelHeight <- max minMessagesHeight (min (screenHeight - dividerThickness - minCavernHeight) messagesPanelHeight)
        let cavernHeight = max minCavernHeight (screenHeight - dividerThickness - messagesPanelHeight)
        let mapLeft = statsPanelWidth + dividerThickness + panelGapWidth
        let rightWidthTiles = max 1 (toTileCellsX rightWidth)
        let cavernHeightTiles = max 1 (toTileCellsY cavernHeight)
        let mapLeftTiles = max 0 ((mapLeft * textCellSize.X) / max 1 tileCellSize.X)
        let dividerTop = cavernHeight

        // The cavern expands to consume whatever space remains after sidebar and log constraints are applied.
        statsPanel.Resize(statsPanelWidth, screenHeight, false)
        statsPanel.Position <- Point(0, 0)

        verticalDivider.Resize(dividerThickness, screenHeight, false)
        verticalDivider.Position <- Point(statsPanelWidth, 0)

        cavernPanel.Resize(rightWidthTiles, cavernHeightTiles, false)
        cavernPanel.Position <- Point(mapLeftTiles, 0)

        horizontalDivider.Resize(rightWidth, dividerThickness, false)
        horizontalDivider.Position <- Point(mapLeft, dividerTop)

        messagesPanel.Resize(rightWidth, messagesPanelHeight, false)
        messagesPanel.Position <- Point(mapLeft, dividerTop + dividerThickness)

        overlayPanel.Resize(rightWidthTiles, cavernHeightTiles, false)
        overlayPanel.Position <- Point(mapLeftTiles, 0)

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

        match projectileAnimation with
        | Some (path, index, _) when index >= 0 && index < path.Length ->
            let position = path[index]

            if isVisible viewportWidth viewportHeight position camera then
                let screenPosition = toScreenPosition camera position
                paintAppearance cavernPanel screenPosition.X screenPosition.Y projectileAppearance
        | _ -> ()

        match regenerationAnimation with
        | Some (position, frames, index, _) when index >= 0 && index < frames.Length ->
            if isVisible viewportWidth viewportHeight position camera then
                let screenPosition = toScreenPosition camera position
                paintAppearance cavernPanel screenPosition.X screenPosition.Y frames[index]
        | _ -> ()

        horizontalDivider.Render()
        messagesPanel.Render(session)
        overlayPanel.Render(session, camera)

    do
        relayout <- layoutPanels
        requestRedraw <- redraw

    let applyIntentAndRedraw intent =
        let priorPlayer = session.State.Player
        let transition = applyIntent services intent session

        match transition.NextSession with
        | Some nextSession ->
            if nextSession.State.World <> session.State.World then
                spokenMonsterIds <- Set.empty
            session <- nextSession

            if session.State.Player.Incarnation > priorPlayer.Incarnation then
                regenerationAnimation <-
                    Some(
                        session.State.Player.Position,
                        regenerationFrames priorPlayer.Glyph session.State.Player.Glyph,
                        0,
                        TimeSpan.Zero
                    )
        | None ->
            Game.Instance.MonoGameInstance.Exit()

        transition.Events
        |> List.iter (function
            | PlaySound _ -> ()
            | SpeakText text -> services.Speak text
            | AnimateProjectile _ -> ())
        
        transition.Events
        |> List.iter (function
            | AnimateProjectile path when not path.IsEmpty ->
                projectileAnimation <- Some(path, 0, TimeSpan.Zero)
            | _ -> ())

        let newlySeenSpeakingMonsters =
            session.State.Monsters
            |> List.filter (fun monster ->
                session.State.VisibleTiles[monster.Position.Y, monster.Position.X]
                && monster.SpeechCue |> Option.isSome
                && not (Set.contains monster.Id spokenMonsterIds))

        newlySeenSpeakingMonsters
        |> List.iter (fun monster ->
            spokenMonsterIds <- Set.add monster.Id spokenMonsterIds
            services.Speak monster.SpeechCue.Value)

        redraw ()

    let tryGetIntent session (keyboard: Keyboard) =
        match session.Modal with
        | NoModal ->
            match pressedRunIntent keyboard with
            | Some intent -> Some intent
            | None ->
                pressedMappedKey keyboard keyTable
                |> Option.map (intentFromKey bindings)
        | TimeShiftPrompt _ ->
            match pressedMappedKey keyboard timeShiftPromptKeyTable with
            | Some key -> Some(intentFromKey bindings key)
            | None ->
                pressedMappedKey keyboard keyTable
                |> Option.map (intentFromKey bindings)
        | _ ->
            pressedMappedKey keyboard keyTable
            |> Option.map (intentFromKey bindings)

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
        match projectileAnimation, regenerationAnimation with
        | Some _, _
        | _, Some _ -> true
        | None, None ->
            match tryGetIntent session keyboard with
            | Some intent ->
                intent |> normalizeIntent session |> applyIntentAndRedraw
                true
            | None -> base.ProcessKeyboard(keyboard)

    override _.Update(delta: TimeSpan) =
        let mutable needsRedraw = false

        match projectileAnimation with
        | Some (path, index, elapsed) ->
            let totalElapsed = elapsed + delta
            let advancedFrames = int (totalElapsed.Ticks / projectileFrameDuration.Ticks)
            let remainingTicks = totalElapsed.Ticks % projectileFrameDuration.Ticks

            if advancedFrames > 0 then
                let nextIndex = index + advancedFrames

                projectileAnimation <-
                    if nextIndex >= path.Length then
                        None
                    else
                        Some(path, nextIndex, TimeSpan.FromTicks remainingTicks)

                needsRedraw <- true
            else
                projectileAnimation <- Some(path, index, totalElapsed)
        | None -> ()

        match regenerationAnimation with
        | Some (position, frames, index, elapsed) ->
            let totalElapsed = elapsed + delta
            let advancedFrames = int (totalElapsed.Ticks / regenerationFrameDuration.Ticks)
            let remainingTicks = totalElapsed.Ticks % regenerationFrameDuration.Ticks

            if advancedFrames > 0 then
                let nextIndex = index + advancedFrames

                regenerationAnimation <-
                    if nextIndex >= frames.Length then
                        None
                    else
                        Some(position, frames, nextIndex, TimeSpan.FromTicks remainingTicks)

                needsRedraw <- true
            else
                regenerationAnimation <- Some(position, frames, index, totalElapsed)
        | None -> ()

        if needsRedraw then
            redraw ()

        base.Update(delta)
