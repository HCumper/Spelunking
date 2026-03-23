module Spelunk.Domain
// pure Domain module with tiles, actors, commands, and update functions.

open Spelunk.Config
open Spelunk.Dungeon

type Position = { X: int; Y: int }

type Tile =
    | Wall
    | Floor
    | StairsDown

type Actor =
    { Id: int
      Name: string
      Position: Position
      Hp: int
      MaxHp: int
      Glyph: char }

type Map =
    { Width: int
      Height: int
      Tiles: Tile[,] }

type GameState =
    { Depth: int
      Map: Map
      Player: Actor
      Monsters: Actor list
      Messages: string list }

type Command =
    | Move of dx: int * dy: int
    | Wait

let private tileAt map point =
    map.Tiles[point.Y, point.X]

let private isWalkable tile =
    match tile with
    | Floor
    | StairsDown -> true
    | Wall -> false

let private actorAt point actors =
    actors |> List.tryFind (fun actor -> actor.Position = point)

let private clampHp actor hp =
    { actor with Hp = max 0 (min actor.MaxHp hp) }

let private addMessage message state =
    { state with Messages = message :: state.Messages |> List.truncate 6 }

let private tryMoveActor map actors actor dx dy =
    let destination =
        { X = actor.Position.X + dx
          Y = actor.Position.Y + dy }

    let inBounds =
        destination.X >= 0
        && destination.X < map.Width
        && destination.Y >= 0
        && destination.Y < map.Height

    if not inBounds then
        Error "The rock wall blocks your way."
    elif not (isWalkable (tileAt map destination)) then
        Error "You bump into rough stone."
    elif actorAt destination actors |> Option.isSome then
        Error "Something is already there."
    else
        Ok { actor with Position = destination }

let private attack target =
    clampHp target (target.Hp - 1)

let private updateMonster state monster =
    let playerPos = state.Player.Position
    let dx = playerPos.X - monster.Position.X
    let dy = playerPos.Y - monster.Position.Y

    let stepX, stepY =
        if abs dx >= abs dy then
            compare dx 0, 0
        else
            0, compare dy 0

    let destination =
        { X = monster.Position.X + stepX
          Y = monster.Position.Y + stepY }

    if destination = playerPos then
        let player = attack state.Player
        { state with Player = player }, Some(sprintf "The %s hits you." monster.Name)
    else
        let otherMonsters = state.Monsters |> List.filter (fun current -> current.Id <> monster.Id)

        match tryMoveActor state.Map otherMonsters monster stepX stepY with
        | Ok moved ->
            let monsters = moved :: otherMonsters |> List.sortBy (fun actor -> actor.Id)
            { state with Monsters = monsters }, None
        | Error _ ->
            state, None

let private runMonsterTurn state =
    ((state, []), state.Monsters)
    ||> List.fold (fun (currentState, notes) monster ->
        let nextState, note = updateMonster currentState monster
        let nextNotes =
            match note with
            | Some text -> text :: notes
            | None -> notes

        nextState, nextNotes)
    |> fun (nextState, notes) ->
        notes
        |> List.rev
        |> List.fold (fun acc note -> addMessage note acc) nextState

let update command state =
    let afterPlayer =
        match command with
        | Wait ->
            addMessage "You wait and listen." state
        | Move (dx, dy) ->
            let destination =
                { X = state.Player.Position.X + dx
                  Y = state.Player.Position.Y + dy }

            match actorAt destination state.Monsters with
            | Some monster ->
                let wounded = attack monster
                let survivors =
                    state.Monsters
                    |> List.filter (fun current -> current.Id <> monster.Id)

                let monsters =
                    if wounded.Hp <= 0 then
                        survivors
                    else
                        wounded :: survivors |> List.sortBy (fun actor -> actor.Id)

                let hitMessage =
                    if wounded.Hp <= 0 then
                        sprintf "You kill the %s." monster.Name
                    else
                        sprintf "You hit the %s." monster.Name

                { state with Monsters = monsters } |> addMessage hitMessage
            | None ->
                match tryMoveActor state.Map state.Monsters state.Player dx dy with
                | Ok movedPlayer -> { state with Player = movedPlayer } |> addMessage "You move."
                | Error message -> addMessage message state

    runMonsterTurn afterPlayer

let private roomCenter (room: Room) =
    { X = room.Left + room.Width / 2
      Y = room.Top + room.Height / 2 }

let private pointInRoom (room: Room) point =
    point.X >= room.Left
    && point.X < room.Left + room.Width
    && point.Y >= room.Top
    && point.Y < room.Top + room.Height

let private fallbackFloorPoint (layout: Layout) =
    seq {
        for y in 0 .. layout.Height - 1 do
            for x in 0 .. layout.Width - 1 do
                if layout.Floors[y, x] then
                    yield { X = x; Y = y }
    }
    |> Seq.tryHead
    |> Option.defaultValue { X = 1; Y = 1 }

let private debugPrintMap map player monsters =
    let glyphAt point =
        if player.Position = point then
            player.Glyph
        else
            monsters
            |> List.tryFind (fun monster -> monster.Position = point)
            |> Option.map (fun monster -> monster.Glyph)
            |> Option.defaultValue (
                match map.Tiles[point.Y, point.X] with
                | Wall -> '#'
                | Floor -> '.'
                | StairsDown -> '>' )

    printfn "Generated map:"

    for y in 0 .. map.Height - 1 do
        let row =
            [ for x in 0 .. map.Width - 1 do
                  glyphAt { X = x; Y = y } ]
            |> Array.ofList
            |> System.String

        printfn "%s" row

let private createDungeon () =
    let layout: Layout = generate (dungeonConfig ())
    let tiles = Array2D.create layout.Height layout.Width Wall

    for y in 0 .. layout.Height - 1 do
        for x in 0 .. layout.Width - 1 do
            if layout.Floors[y, x] then
                tiles[y, x] <- Floor

    let stairPoint =
        layout.Rooms
        |> List.tryLast
        |> Option.map roomCenter
        |> Option.defaultValue (fallbackFloorPoint layout)

    tiles[stairPoint.Y, stairPoint.X] <- StairsDown

    { Width = layout.Width
      Height = layout.Height
      Tiles = tiles },
    layout.Rooms

let createMap () =
    createDungeon () |> fst

let initialState () =
    let map, rooms = createDungeon ()
    let spawnPoint =
        rooms
        |> List.tryHead
        |> Option.map roomCenter
        |> Option.defaultValue { X = 1; Y = 1 }

    let monsterSpawns =
        rooms
        |> List.skip 1
        |> List.filter (fun room -> not (pointInRoom room spawnPoint))
        |> List.map roomCenter

    let crawlerSpawn =
        monsterSpawns
        |> List.tryHead
        |> Option.defaultValue spawnPoint

    let sentrySpawn =
        monsterSpawns
        |> List.tryItem 1
        |> Option.defaultValue spawnPoint

    let player =
        { Id = 0
          Name = "scavenger"
          Position = spawnPoint
          Hp = 10
          MaxHp = 10
          Glyph = '@' }

    let monsters =
        [ { Id = 1
            Name = "crawler"
            Position = crawlerSpawn
            Hp = 3
            MaxHp = 3
            Glyph = 'c' }
          { Id = 2
            Name = "sentry"
            Position = sentrySpawn
            Hp = 4
            MaxHp = 4
            Glyph = 's' } ]

    debugPrintMap map player monsters

    { Depth = 1
      Map = map
      Player = player
      Monsters = monsters
      Messages =
        [ "Immutable game state. Mutable rendering stays outside the domain."
          "Move with WASD or arrow keys. Press Space to wait. Press Q to quit." ] }
