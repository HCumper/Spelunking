(* Composes world generation, simulation, and visibility into the game-facing state transitions. *)
module Spelunk.Domain
open Spelunk.Config
open Spelunk.Model
open Spelunk.Simulation
open Spelunk.Visibility
open Spelunk.World

let createMap () =
    createDungeon () |> fst

let private advanceTurn state =
    let nextTurnCount = state.TurnCount + 1

    if nextTurnCount % 20 = 0 && state.Player.Hp < state.Player.MaxHp then
        { state with
            TurnCount = nextTurnCount
            Player = { state.Player with Hp = min state.Player.MaxHp (state.Player.Hp + 10) } }
    else
        { state with TurnCount = nextTurnCount }

let private chooseWeightedMonsterTemplate
    (world: int)
    (templates: MonsterTemplate list)
    (random: System.Random)
    =
    let eligibleTemplates =
        templates
        |> List.filter (fun template ->
            template.Frequency > 0
            && world >= template.MinWorld
            && world <= template.MaxWorld)

    match eligibleTemplates with
    | [] -> None
    | _ ->
        let totalWeight = eligibleTemplates |> List.sumBy (fun template -> template.Frequency)
        let roll = random.Next(1, totalWeight + 1)

        let rec pick remainingWeight remainingTemplates =
            match remainingTemplates with
            | [] -> None
            | (template: MonsterTemplate) :: tail ->
                let nextWeight = remainingWeight + template.Frequency

                if roll <= nextWeight then
                    Some template
                else
                    pick nextWeight tail

        pick 0 eligibleTemplates

let private createWorldState world player weapon messages =
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

    let monsters =
        let templates = monsterTemplates ()
        let spawn = spawnSettings ()
        let random = System.Random.Shared

        if List.isEmpty templates then
            []
        else
            let requestedCount =
                monsterSpawns.Length
                |> float
                |> (*) spawn.MonsterRoomDensity
                |> ceil
                |> int
                |> max 0
                |> min spawn.MaxMonsters

            let selectedSpawns =
                monsterSpawns
                |> List.sortBy (fun _ -> random.Next())
                |> List.truncate requestedCount

            selectedSpawns
            |> List.indexed
            |> List.choose (fun (index, spawnPoint) ->
                chooseWeightedMonsterTemplate world templates random
                |> Option.map (fun (template: MonsterTemplate) ->
                    { Id = index + 1
                      Name = template.Name
                      Position = spawnPoint
                      Hp = template.MaxHp
                      MaxHp = template.MaxHp
                      Speed = template.Speed
                      Strength = template.Strength
                      Energy = 0
                      Glyph =
                        match template.Glyph with
                        | null
                        | "" -> '?'
                        | value -> value[0]
                      SpeechCue = template.SpeechCue }))

    { World = world
      TurnCount = 0
      Map = map
      Player = { player with Position = spawnPoint; Energy = 0 }
      PlayerWeapon = weapon
      Monsters = monsters
      VisibleTiles = Array2D.create map.Height map.Width false
      ExploredTiles = Array2D.create map.Height map.Width false
      Messages = messages }
    |> computeVisibility

let private enterTardis state =
    createWorldState
        (state.World + 1)
        state.Player
        state.PlayerWeapon
        [ "The Tardis hurtles through time and space and re-materialized." ]

let initialState () =
    let startingWeaponTemplate = defaultWeaponTemplate ()
    let player =
        { Id = 0
          Name = "scavenger"
          Position = { X = 1; Y = 1 }
          Hp = 100
          MaxHp = 100
          Speed = 100
          Strength = 100
          Energy = 0
          Glyph = '@'
          SpeechCue = None }

    let initial =
        createWorldState
            1
            player
            { Name = startingWeaponTemplate.Name
              Range = startingWeaponTemplate.Range
              Damage = startingWeaponTemplate.Damage
              Ammo = startingWeaponTemplate.Ammo }
            []

    { initial with
        Messages =
            "Move with WASD or arrow keys. Press Space to wait. Press Q to quit."
            :: initial.Messages }

let update command state =
    let resolved = Simulation.update command state

    if resolved.Map.Tiles[resolved.Player.Position.Y, resolved.Player.Position.X] = Tardis then
        enterTardis resolved
    else
        resolved
        |> runMonsterTurn
        |> advanceTurn
        |> computeVisibility
