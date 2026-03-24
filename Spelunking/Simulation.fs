module Spelunk.Simulation

open Spelunk.Config
open Spelunk.Model

let tileAt map point =
    map.Tiles[point.Y, point.X]

let isWalkable tile =
    match tile with
    | Floor
    | StairsDown -> true
    | Wall -> false

let actorAt point actors =
    actors |> List.tryFind (fun actor -> actor.Position = point)

let clampHp actor hp =
    { actor with Hp = max 0 (min actor.MaxHp hp) }

let addMessage message state =
    { state with Messages = message :: state.Messages |> List.truncate 6 }

let tryMoveActor map actors actor dx dy =
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

let attack target =
    clampHp target (target.Hp - (combatSettings ()).MonsterAttackDamage)

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

let runMonsterTurn state =
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
