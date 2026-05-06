(* Resolves turn-by-turn combat, ranged attacks, movement, and monster AI. *)
module Spelunk.Simulation

open Spelunk.Combat
open Spelunk.Messages
open Spelunk.Model

type UpdateResult =
    { State: GameState
      ProjectilePaths: Position list list }

type MonsterTurnResult =
    { State: GameState
      Notes: string list
      ProjectilePaths: Position list list }

let tileAt map point =
    map.Tiles[point.Y, point.X]

let isWalkable tile =
    match tile with
    | Floor
    | Tardis -> true
    | Wall -> false

let actorAt point actors =
    actors |> List.tryFind (fun actor -> actor.Position = point)

let private actionThreshold = 100

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

let private updateMonsterInState monster state =
    { state with
        Monsters =
            state.Monsters
            |> List.map (fun current -> if current.Id = monster.Id then monster else current) }

let private removeMonsterById monsterId state =
    { state with
        Monsters = state.Monsters |> List.filter (fun current -> current.Id <> monsterId) }

let private actorAlongPath shooterId point state =
    if state.Player.Id <> shooterId && state.Player.Position = point then
        Some state.Player
    else
        state.Monsters
        |> List.tryFind (fun actor -> actor.Id <> shooterId && actor.Position = point)

let private projectilePath shooterId origin range target state =
    let stepX = compare (target.X - origin.X) 0
    let stepY = compare (target.Y - origin.Y) 0

    let inBounds point =
        point.X >= 0
        && point.X < state.Map.Width
        && point.Y >= 0
        && point.Y < state.Map.Height

    let rec loop point stepsRemaining acc =
        if stepsRemaining <= 0 || (stepX = 0 && stepY = 0) then
            List.rev acc
        else
            let nextPoint =
                { X = point.X + stepX
                  Y = point.Y + stepY }

            if not (inBounds nextPoint) then
                List.rev acc
            else
                let nextAcc = nextPoint :: acc

                match tileAt state.Map nextPoint with
                | Wall ->
                    List.rev nextAcc
                | _ ->
                    match actorAlongPath shooterId nextPoint state with
                    | Some _ -> List.rev nextAcc
                    | None -> loop nextPoint (stepsRemaining - 1) nextAcc

    loop origin range []

let private firstActorBeyondRange shooterId origin range target state =
    let stepX = compare (target.X - origin.X) 0
    let stepY = compare (target.Y - origin.Y) 0

    let inBounds point =
        point.X >= 0
        && point.X < state.Map.Width
        && point.Y >= 0
        && point.Y < state.Map.Height

    let rec skipToRange point stepsRemaining =
        if stepsRemaining <= 0 || (stepX = 0 && stepY = 0) then
            point
        else
            let nextPoint =
                { X = point.X + stepX
                  Y = point.Y + stepY }

            if not (inBounds nextPoint) then
                point
            else
                skipToRange nextPoint (stepsRemaining - 1)

    let rec find point =
        let nextPoint =
            { X = point.X + stepX
              Y = point.Y + stepY }

        if not (inBounds nextPoint) then
            None
        else
            match tileAt state.Map nextPoint with
            | Wall -> None
            | _ ->
                match actorAlongPath shooterId nextPoint state with
                | Some actor -> Some actor
                | None -> find nextPoint

    skipToRange origin range |> find

let private withUpdatedRangedWeapon shooterId weapon state =
    if shooterId = state.Player.Id then
        { state with Player = { state.Player with RangedWeapon = weapon } }
    else
        state.Monsters
        |> List.tryFind (fun monster -> monster.Id = shooterId)
        |> Option.map (fun monster -> updateMonsterInState { monster with RangedWeapon = weapon } state)
        |> Option.defaultValue state

let private tryRegeneratePlayer player =
    if player.Hp > 0 || player.RegenerationsRemaining <= 0 then
        None
    else
        let nextIncarnation = player.Incarnation + 1

        Some
            { player with
                Hp = player.MaxHp
                Glyph = playerGlyphForIncarnation nextIncarnation
                Incarnation = nextIncarnation
                RegenerationsRemaining = player.RegenerationsRemaining - 1 }

let private applyRangedHit shooterId shooterName weapon targetPoint targetActor state =
    let wounded = attackWithWeapon weapon targetActor
    let stateWithAmmo = withUpdatedRangedWeapon shooterId (addAmmo weapon -1) state

    if targetActor.Id = state.Player.Id then
        match tryRegeneratePlayer wounded with
        | Some regenerated ->
            { stateWithAmmo with Player = regenerated }, sprintf "%s shoots you and triggers your regeneration." shooterName
        | None ->
            let message =
                if wounded.Hp <= 0 then
                    sprintf "%s kills you." shooterName
                else
                    sprintf "%s shoots you." shooterName

            { stateWithAmmo with Player = wounded }, message
    else
        let woundedState =
            if wounded.Hp <= 0 then
                removeMonsterById targetActor.Id stateWithAmmo
            else
                updateMonsterInState wounded stateWithAmmo

        let player =
            if shooterId = state.Player.Id && wounded.Hp <= 0 then
                clampHp woundedState.Player (woundedState.Player.Hp + killBoost targetActor.MaxHp)
            else
                woundedState.Player

        let message =
            if shooterId = state.Player.Id then
                if wounded.Hp <= 0 then
                    sprintf "You blast the %s apart." targetActor.Name
                else
                    sprintf "You shoot the %s." targetActor.Name
            else if targetPoint = state.Player.Position then
                if wounded.Hp <= 0 then
                    sprintf "%s shoots at you and blasts the %s apart." shooterName targetActor.Name
                else
                    sprintf "%s shoots at you but hits the %s." shooterName targetActor.Name
            else if wounded.Hp <= 0 then
                sprintf "%s blasts the %s apart." shooterName targetActor.Name
            else
                sprintf "%s shoots the %s." shooterName targetActor.Name

        { woundedState with Player = player }, message

let private tryFire shooterId shooterName origin weapon target state =
    match weapon.Ammo with
    | Some ammo when ammo <= 0 ->
        state, Some(sprintf "%s is out of ammo." weapon.Name), None
    | _ when chebyshevDistance origin target > weapon.Range ->
        state, Some(sprintf "%s cannot reach that far." weapon.Name), None
    | _ ->
        let path = projectilePath shooterId origin weapon.Range target state

        let rec resolve remaining currentState =
            match remaining with
            | [] ->
                let nextState = withUpdatedRangedWeapon shooterId (addAmmo weapon -1) currentState
                let message =
                    match firstActorBeyondRange shooterId origin weapon.Range target currentState with
                    | Some actor when actor.Id = currentState.Player.Id ->
                        sprintf "%s's %s falls short of you." shooterName weapon.Name
                    | Some actor ->
                        sprintf "%s's %s falls short of the %s." shooterName weapon.Name actor.Name
                    | None ->
                        sprintf "%s's %s misses." shooterName weapon.Name

                nextState, Some message, Some path
            | point :: tail ->
                match tileAt currentState.Map point with
                | Wall ->
                    let nextState = withUpdatedRangedWeapon shooterId (addAmmo weapon -1) currentState
                    nextState, Some(sprintf "%s's %s blasts the wall." shooterName weapon.Name), Some path
                | _ ->
                    match actorAlongPath shooterId point currentState with
                    | Some targetActor ->
                        let nextState, message = applyRangedHit shooterId shooterName weapon target targetActor currentState
                        nextState, Some message, Some path
                    | None ->
                        resolve tail currentState

        resolve path state

let private tryPlayerFireAt target state =
    let nextState, message, projectilePath =
        tryFire
            state.Player.Id
            "You"
            state.Player.Position
            state.Player.RangedWeapon
            target
            state

    let stateWithMessage =
        match message with
        | Some text when text.StartsWith("You's ") ->
            addMessage (text.Replace("You's ", "Your ")) nextState
        | Some text ->
            addMessage text nextState
        | None ->
            nextState

    { State = stateWithMessage
      ProjectilePaths = projectilePath |> Option.toList }

let private tryMonsterFireAt monster target state =
    let nextState, message, projectilePath =
        tryFire
            monster.Id
            (sprintf "The %s" monster.Name)
            monster.Position
            monster.RangedWeapon
            target
            state

    nextState, message, (projectilePath |> Option.toList)

let private canUseRangedWeapon weapon origin target =
    weapon.Range > 1
    && chebyshevDistance origin target <= weapon.Range
    &&
    match weapon.Ammo with
    | Some ammo -> ammo > 0
    | None -> true

let private updateMonster state monster =
    let playerPos = state.Player.Position
    let dx = playerPos.X - monster.Position.X
    let dy = playerPos.Y - monster.Position.Y

    let stepX, stepY =
        compare dx 0, compare dy 0

    let movementOptions =
        match stepX, stepY with
        | 0, 0 -> []
        | 0, _ -> [ stepX, stepY ]
        | _, 0 -> [ stepX, stepY ]
        | _ -> [ stepX, stepY; stepX, 0; 0, stepY ]

    let destination =
        { X = monster.Position.X + stepX
          Y = monster.Position.Y + stepY }

    if destination = playerPos then
        let wounded = attackWithWeapon monster.MeleeWeapon state.Player

        match tryRegeneratePlayer wounded with
        | Some regenerated ->
            { state with Player = regenerated }, Some(sprintf "The %s hits you and triggers your regeneration." monster.Name), []
        | None ->
            { state with Player = wounded }, Some(sprintf "The %s hits you." monster.Name), []
    elif canUseRangedWeapon monster.RangedWeapon monster.Position playerPos then
        let nextState, note, projectilePaths = tryMonsterFireAt monster playerPos state
        nextState, note, projectilePaths
    else
        let otherMonsters = state.Monsters |> List.filter (fun current -> current.Id <> monster.Id)
        let movedMonster =
            movementOptions
            |> List.tryPick (fun (candidateX, candidateY) ->
                match tryMoveActor state.Map otherMonsters monster candidateX candidateY with
                | Ok moved -> Some moved
                | Error _ -> None)

        match movedMonster with
        | Some moved ->
            let monsters = moved :: otherMonsters |> List.sortBy (fun actor -> actor.Id)
            { state with Monsters = monsters }, None, []
        | None ->
            state, None, []

let private runMonsterActions state monster =
    let rec loop currentState currentMonster notes projectilePaths =
        if currentState.Player.Hp <= 0 || currentMonster.Energy < actionThreshold then
            currentState, currentMonster, notes, projectilePaths
        else
            // Monsters spend energy in 100-point chunks, so speed below or above 100 acts less or more often than the player.
            let actingMonster = { currentMonster with Energy = currentMonster.Energy - actionThreshold }
            let monsterState =
                { currentState with
                    Monsters =
                        currentState.Monsters
                        |> List.map (fun existing ->
                            if existing.Id = actingMonster.Id then actingMonster else existing) }

            let nextState, note, newProjectilePaths = updateMonster monsterState actingMonster
            let nextMonster =
                nextState.Monsters
                |> List.tryFind (fun existing -> existing.Id = actingMonster.Id)

            let nextNotes =
                match note with
                | Some text -> text :: notes
                | None -> notes

            let nextProjectilePaths =
                projectilePaths @ newProjectilePaths

            match nextMonster with
            | Some aliveMonster -> loop nextState aliveMonster nextNotes nextProjectilePaths
            | None -> nextState, actingMonster, nextNotes, nextProjectilePaths

    let chargedMonster = { monster with Energy = monster.Energy + max 0 monster.Speed }
    let chargedState =
        { state with
            Monsters =
                state.Monsters
                |> List.map (fun existing ->
                    if existing.Id = chargedMonster.Id then chargedMonster else existing) }

    loop chargedState chargedMonster [] []

let runMonsterTurnDetailed state =
    ((state, [], []), state.Monsters)
    ||> List.fold (fun (currentState, notes, projectilePaths) monster ->
        match currentState.Monsters |> List.tryFind (fun existing -> existing.Id = monster.Id) with
        | Some nextMonster ->
            let nextState, _, monsterNotes, monsterProjectilePaths = runMonsterActions currentState nextMonster
            let nextNotes =
                monsterNotes |> List.rev |> List.append notes
            let nextProjectilePaths =
                projectilePaths @ monsterProjectilePaths

            nextState, nextNotes, nextProjectilePaths
        | None ->
            currentState, notes, projectilePaths)
    |> fun (nextState, notes, projectilePaths) ->
        { State = nextState
          Notes = notes
          ProjectilePaths = projectilePaths }

let runMonsterTurn state =
    let result = runMonsterTurnDetailed state

    result.Notes
    |> List.fold (fun acc note -> addMessage note acc) result.State

let updateDetailed command state =
    match command with
    | Wait ->
        { State = state; ProjectilePaths = [] }
    | FireAt target ->
        tryPlayerFireAt target state
    | Move (dx, dy) ->
        let destination =
            { X = state.Player.Position.X + dx
              Y = state.Player.Position.Y + dy }

        match actorAt destination state.Monsters with
        | Some monster ->
            let wounded = attackWithWeapon state.Player.MeleeWeapon monster
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

            { State = { state with Player = player; Monsters = monsters } |> addMessage hitMessage
              ProjectilePaths = [] }
        | None ->
            match tryMoveActor state.Map state.Monsters state.Player dx dy with
            | Ok movedPlayer ->
                { State = { state with Player = movedPlayer }
                  ProjectilePaths = [] }
            | Error message ->
                { State = addMessage message state
                  ProjectilePaths = [] }

let update command state =
    (updateDetailed command state).State
