module Spelunk.Domain
open Spelunk.Config
open Spelunk.Model
open Spelunk.Simulation
open Spelunk.Visibility
open Spelunk.World

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

    let player =
        { Id = 0
          Name = "scavenger"
          Position = spawnPoint
          Hp = 10
          MaxHp = 10
          Glyph = '@' }

    let monsters =
        let templates = monsterTemplates ()
        let boundedSpawns = monsterSpawns |> List.truncate templates.Length

        List.zip templates boundedSpawns
        |> List.map (fun (template, spawn) ->
            { Id = template.Id
              Name = template.Name
              Position = spawn
              Hp = template.MaxHp
              MaxHp = template.MaxHp
              Glyph =
                match template.Glyph with
                | null
                | "" -> '?'
                | value -> value[0] })

    { Depth = 1
      Map = map
      Player = player
      Monsters = monsters
      VisibleTiles = Array2D.create map.Height map.Width false
      ExploredTiles = Array2D.create map.Height map.Width false
      Messages =
        [ "Immutable game state. Mutable rendering stays outside the domain."
          "Move with WASD or arrow keys. Press Space to wait. Press Q to quit." ] }
    |> computeVisibility

let update command state =
    Simulation.update command state
    |> runMonsterTurn
    |> computeVisibility
