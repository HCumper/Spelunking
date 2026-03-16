module Spelunk.Domain
// pure Domain module with tiles, actors, commands, and update functions.

type Point = { X: int; Y: int }

type Tile =
    | Wall
    | Floor
    | StairsDown

type Actor =
    { Id: int
      Name: string
      Position: Point
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

let createMap () =
    let width = 40
    let height = 20
    let tiles = Array2D.create height width Wall

    for y in 1 .. height - 2 do
        for x in 1 .. width - 2 do
            tiles[y, x] <- Floor

    for x in 10 .. 28 do
        tiles[8, x] <- Wall

    tiles[8, 18] <- Floor
    tiles[height - 3, width - 3] <- StairsDown

    { Width = width
      Height = height
      Tiles = tiles }

let initialState () =
    let map = createMap ()
    let player =
        { Id = 0
          Name = "scavenger"
          Position = { X = 2; Y = 2 }
          Hp = 10
          MaxHp = 10
          Glyph = '@' }

    let monsters =
        [ { Id = 1
            Name = "crawler"
            Position = { X = 12; Y = 4 }
            Hp = 3
            MaxHp = 3
            Glyph = 'c' }
          { Id = 2
            Name = "sentry"
            Position = { X = 26; Y = 14 }
            Hp = 4
            MaxHp = 4
            Glyph = 's' } ]

    { Depth = 1
      Map = map
      Player = player
      Monsters = monsters
      Messages =
        [ "Immutable game state. Mutable rendering stays outside the domain."
          "Move with WASD or arrow keys. Press Space to wait. Press Q to quit." ] }
