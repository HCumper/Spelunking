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

    if nextTurnCount % 10 = 0 && state.Player.Hp < state.Player.MaxHp then
        { state with
            TurnCount = nextTurnCount
            Player = { state.Player with Hp = state.Player.Hp + 1 } }
    else
        { state with TurnCount = nextTurnCount }

let private chooseWeightedMonsterTemplate
    (depth: int)
    (templates: MonsterTemplate list)
    (random: System.Random)
    =
    let eligibleTemplates =
        templates
        |> List.filter (fun template ->
            template.Frequency > 0
            && depth >= template.MinDepth
            && depth <= template.MaxDepth)

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

let initialState () =
    let depth = 1
    let map, rooms = createDungeon ()
    let startingWeaponTemplate = defaultWeaponTemplate ()
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

    let player =
        { Id = 0
          Name = "scavenger"
          Position = spawnPoint
          Hp = 10
          MaxHp = 10
          Speed = 10
          Strength = 10
          Energy = 0
          Glyph = '@' }

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
                chooseWeightedMonsterTemplate depth templates random
                |> Option.map (fun template ->
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
                        | value -> value[0] }))

    { Depth = depth
      TurnCount = 0
      Map = map
      Player = player
      PlayerWeapon =
        { Name = startingWeaponTemplate.Name
          Range = startingWeaponTemplate.Range
          Damage = startingWeaponTemplate.Damage
          Ammo = startingWeaponTemplate.Ammo }
      Monsters = monsters
      VisibleTiles = Array2D.create map.Height map.Width false
      ExploredTiles = Array2D.create map.Height map.Width false
      Messages =
        [ 
          "Move with WASD or arrow keys. Press Space to wait. Press Q to quit." ] }
    |> computeVisibility

let update command state =
    Simulation.update command state
    |> runMonsterTurn
    |> advanceTurn
    |> computeVisibility
