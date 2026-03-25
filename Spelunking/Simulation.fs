(* Resolves turn-by-turn combat, ranged attacks, movement, and monster AI. *)
module Spelunk.Simulation

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

let private actionThreshold = 10

let private damageFor attacker =
    // Strength 10 corresponds to baseline 1 damage; larger values scale upward in whole points.
    max 1 ((attacker.Strength + actionThreshold - 1) / actionThreshold)

let private addAmmo weapon delta =
    { weapon with
        Ammo =
            match weapon.Ammo with
            | Some ammo -> Some(max 0 (ammo + delta))
            | None -> None }

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

let attack attacker target =
    clampHp target (target.Hp - damageFor attacker)

let private attackWithWeapon weapon target =
    clampHp target (target.Hp - max 1 weapon.Damage)

let private killBoost maxHp =
    (maxHp + 1) / 2

let private chebyshevDistance a b =
    max (abs (a.X - b.X)) (abs (a.Y - b.Y))

let private lineBetween startPoint endPoint =
    let dx = abs (endPoint.X - startPoint.X)
    let dy = abs (endPoint.Y - startPoint.Y)
    let sx = if startPoint.X < endPoint.X then 1 elif startPoint.X > endPoint.X then -1 else 0
    let sy = if startPoint.Y < endPoint.Y then 1 elif startPoint.Y > endPoint.Y then -1 else 0

    let rec loop x y err points =
        let nextPoints = { X = x; Y = y } :: points

        if x = endPoint.X && y = endPoint.Y then
            List.rev nextPoints
        else
            let e2 = err * 2
            let nextX, nextErr =
                if e2 > -dy then x + sx, err - dy else x, err
            let nextY, finalErr =
                if e2 < dx then y + sy, nextErr + dx else y, nextErr

            loop nextX nextY finalErr nextPoints

    loop startPoint.X startPoint.Y (dx - dy) []

let private tryFireAt target state =
    let weapon = state.PlayerWeapon

    match weapon.Ammo with
    | Some ammo when ammo <= 0 ->
        addMessage (sprintf "%s is out of ammo." weapon.Name) state
    | _ ->
        let playerPos = state.Player.Position

        if chebyshevDistance playerPos target > weapon.Range then
            addMessage (sprintf "%s cannot reach that far." weapon.Name) state
        else
            // Hitscan shots stop at the first wall or monster along the traced line.
            let path =
                lineBetween playerPos target
                |> List.tail

            let rec resolve remaining =
                match remaining with
                | [] ->
                    { state with PlayerWeapon = addAmmo weapon -1 }
                    |> addMessage (sprintf "Your %s misses." weapon.Name)
                | point :: tail ->
                    match tileAt state.Map point with
                    | Wall ->
                        { state with PlayerWeapon = addAmmo weapon -1 }
                        |> addMessage (sprintf "Your %s blasts the wall." weapon.Name)
                    | _ ->
                        match actorAt point state.Monsters with
                        | Some monster ->
                            let wounded = attackWithWeapon weapon monster
                            let survivors =
                                state.Monsters
                                |> List.filter (fun current -> current.Id <> monster.Id)

                            let monsters =
                                if wounded.Hp <= 0 then
                                    survivors
                                else
                                    wounded :: survivors |> List.sortBy (fun actor -> actor.Id)

                            let player =
                                if wounded.Hp <= 0 then
                                    clampHp state.Player (state.Player.Hp + killBoost monster.MaxHp)
                                else
                                    state.Player

                            let message =
                                if wounded.Hp <= 0 then
                                    sprintf "You blast the %s apart." monster.Name
                                else
                                    sprintf "You shoot the %s." monster.Name

                            { state with
                                Player = player
                                PlayerWeapon = addAmmo weapon -1
                                Monsters = monsters }
                            |> addMessage message
                        | None -> resolve tail

            resolve path

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
        let player = attack monster state.Player
        { state with Player = player }, Some(sprintf "The %s hits you." monster.Name)
    else
        let otherMonsters = state.Monsters |> List.filter (fun current -> current.Id <> monster.Id)

        match tryMoveActor state.Map otherMonsters monster stepX stepY with
        | Ok moved ->
            let monsters = moved :: otherMonsters |> List.sortBy (fun actor -> actor.Id)
            { state with Monsters = monsters }, None
        | Error _ ->
            state, None

let private runMonsterActions state monster =
    let rec loop currentState currentMonster notes =
        if currentState.Player.Hp <= 0 || currentMonster.Energy < actionThreshold then
            currentState, currentMonster, notes
        else
            // Monsters spend energy in 10-point chunks, so speed below or above 10 acts less or more often than the player.
            let actingMonster = { currentMonster with Energy = currentMonster.Energy - actionThreshold }
            let monsterState =
                { currentState with
                    Monsters =
                        currentState.Monsters
                        |> List.map (fun existing ->
                            if existing.Id = actingMonster.Id then actingMonster else existing) }

            let nextState, note = updateMonster monsterState actingMonster
            let nextMonster =
                nextState.Monsters
                |> List.tryFind (fun existing -> existing.Id = actingMonster.Id)

            let nextNotes =
                match note with
                | Some text -> text :: notes
                | None -> notes

            match nextMonster with
            | Some aliveMonster -> loop nextState aliveMonster nextNotes
            | None -> nextState, actingMonster, nextNotes

    let chargedMonster = { monster with Energy = monster.Energy + max 0 monster.Speed }
    let chargedState =
        { state with
            Monsters =
                state.Monsters
                |> List.map (fun existing ->
                    if existing.Id = chargedMonster.Id then chargedMonster else existing) }

    loop chargedState chargedMonster []

let runMonsterTurn state =
    ((state, []), state.Monsters)
    ||> List.fold (fun (currentState, notes) monster ->
        let nextMonster =
            currentState.Monsters
            |> List.tryFind (fun existing -> existing.Id = monster.Id)
            |> Option.defaultValue monster

        let nextState, _, monsterNotes = runMonsterActions currentState nextMonster
        let nextNotes =
            monsterNotes |> List.rev |> List.append notes

        nextState, nextNotes)
    |> fun (nextState, notes) ->
        notes
        |> List.rev
        |> List.fold (fun acc note -> addMessage note acc) nextState

let update command state =
    match command with
    | Wait ->
        addMessage "You wait and listen." state
    | FireAt target ->
        tryFireAt target state
    | Move (dx, dy) ->
        let destination =
            { X = state.Player.Position.X + dx
              Y = state.Player.Position.Y + dy }

        match actorAt destination state.Monsters with
        | Some monster ->
            let wounded = attack state.Player monster
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

            let player =
                if wounded.Hp <= 0 then
                    clampHp state.Player (state.Player.Hp + killBoost monster.MaxHp)
                else
                    state.Player

            { state with Player = player; Monsters = monsters } |> addMessage hitMessage
        | None ->
            match tryMoveActor state.Map state.Monsters state.Player dx dy with
            | Ok movedPlayer -> { state with Player = movedPlayer }
            | Error message -> addMessage message state
