module Spelunking.Tests

open System
open System.IO
open FsUnit.Xunit
open Spelunk.Application
open Spelunk.Combat
open Spelunk.Model
open Spelunk.Save
open Spelunk.Services
open Spelunk.SessionActions
open Spelunk.SessionHistory
open Spelunk.Simulation
open Spelunk.Visibility
open Xunit

let private actor id name x y hp maxHp speed strength energy glyph =
    { Id = id
      Name = name
      Position = { X = x; Y = y }
      Hp = hp
      MaxHp = maxHp
      Speed = speed
      Strength = strength
      Energy = energy
      Glyph = glyph }

let private weapon name range damage ammo =
    { Name = name
      Range = range
      Damage = damage
      Ammo = ammo }

let private map width height defaultTile =
    { Width = width
      Height = height
      Tiles = Array2D.create height width defaultTile }

let private state width height =
    let dungeon = map width height Floor

    { Depth = 1
      TurnCount = 0
      Map = dungeon
      Player = actor 0 "scavenger" 1 1 10 10 100 100 0 '@'
      PlayerWeapon = weapon "Rusty raygun" 8 2 None
      Monsters = []
      VisibleTiles = Array2D.create height width false
      ExploredTiles = Array2D.create height width false
      Messages = [] }

let private session gameState =
    { State = gameState
      Modal = NoModal
      History = [] }

let private testServices loadedState loadedHistory =
    { SaveGame = fun _ -> ()
      TryLoadGame = fun () -> Some(loadedState, loadedHistory) }

let private inertServices =
    { SaveGame = fun _ -> ()
      TryLoadGame = fun () -> None }

let private failingSaveServices : Services =
    { SaveGame = fun (_: GameState * GameState list) -> raise (Exception("disk full"))
      TryLoadGame = fun () -> None }

let private failingLoadServices : Services =
    { SaveGame = fun _ -> ()
      TryLoadGame = fun () -> raise (Exception("corrupt file")) }

let private saveFilePath () =
    Path.Combine(AppContext.BaseDirectory, "Data", "savegame.json")

let private assertStateEqual actual expected =
    actual.Depth |> should equal expected.Depth
    actual.TurnCount |> should equal expected.TurnCount
    actual.Map |> should equal expected.Map
    actual.Player |> should equal expected.Player
    actual.PlayerWeapon |> should equal expected.PlayerWeapon
    actual.Monsters |> should equal expected.Monsters
    actual.VisibleTiles |> should equal expected.VisibleTiles
    actual.ExploredTiles |> should equal expected.ExploredTiles
    actual.Messages |> should equal expected.Messages

[<Fact>]
let ``damageFor maps strength bands to whole-point damage`` () =
    let weak = actor 1 "weak" 0 0 5 5 100 1 0 'w'
    let baseline = actor 2 "baseline" 0 0 5 5 100 100 0 'b'
    let strong = actor 3 "strong" 0 0 5 5 100 101 0 's'
    let elite = actor 4 "elite" 0 0 5 5 100 200 0 'e'

    damageFor weak |> should equal 1
    damageFor baseline |> should equal 1
    damageFor strong |> should equal 2
    damageFor elite |> should equal 2

[<Fact>]
let ``save and load round-trip game state and history`` () =
    let original =
        { state 4 4 with
            TurnCount = 7
            Player = actor 0 "scavenger" 2 1 8 10 100 100 0 '@'
            Monsters = [ actor 1 "Cyberman" 3 3 2 3 50 70 40 'C' ]
            Messages = [ "You shoot the Cyberman."; "The Cyberman hits you." ] }

    let original =
        { original with
            VisibleTiles =
                array2D [ [ true; true; false; false ]
                          [ true; true; false; false ]
                          [ false; false; false; false ]
                          [ false; false; false; false ] ]
            ExploredTiles =
                array2D [ [ true; true; true; false ]
                          [ true; true; true; false ]
                          [ false; false; false; false ]
                          [ false; false; false; false ] ] }

    let prior = { original with TurnCount = 6; Messages = [ "Previous turn." ] }
    let path = saveFilePath ()

    if File.Exists path then
        File.Delete path

    saveGame original [ prior ]

    match tryLoadGame () with
    | Some (loadedState, loadedHistory) ->
        assertStateEqual loadedState original
        loadedHistory.Length |> should equal 1
        assertStateEqual loadedHistory.Head prior
    | None ->
        failwith "Expected save file to load."

[<Fact>]
let ``recordAction only stores turn-taking commands`` () =
    let baseSession = session (state 3 3)

    let moved = recordAction (Move(1, 0)) baseSession
    let waited = recordAction Wait baseSession
    let fired = recordAction (FireAt { X = 2; Y = 2 }) baseSession

    moved.History.Length |> should equal 1
    waited.History.Length |> should equal 1
    fired.History.Length |> should equal 1

[<Fact>]
let ``rewindSession restores the requested earlier state`` () =
    let latest = { state 3 3 with TurnCount = 10; Messages = [ "Latest" ] }
    let fiveTurnsBack = { latest with TurnCount = 5; Messages = [ "Five turns back" ] }
    let twoTurnsBack = { latest with TurnCount = 8; Messages = [ "Two turns back" ] }

    let rewound =
        { State = latest
          Modal = TimeShiftPrompt "5"
          History = [ twoTurnsBack; fiveTurnsBack ] }
        |> rewindSession 2

    rewound.State.TurnCount |> should equal 5
    rewound.State.Messages.Head.StartsWith("The time shifter drags you back 2 turns.") |> should equal true
    rewound.History.Length |> should equal 0
    rewound.Modal |> should equal NoModal

[<Fact>]
let ``computeVisibility blocks line of sight through walls and retains explored tiles`` () =
    let dungeon = map 5 3 Floor
    dungeon.Tiles[1, 2] <- Wall

    let initial =
        { state 5 3 with
            Map = dungeon
            Player = actor 0 "scavenger" 0 1 10 10 100 100 0 '@'
            ExploredTiles =
                array2D [ [ false; false; false; false; false ]
                          [ false; true; false; false; true ]
                          [ false; false; false; false; false ] ] }

    let visible = computeVisibility initial

    visible.VisibleTiles[1, 1] |> should equal true
    visible.VisibleTiles[1, 4] |> should equal false
    visible.ExploredTiles[1, 1] |> should equal true
    visible.ExploredTiles[1, 4] |> should equal true

[<Fact>]
let ``time shifter prompt confirm rewinds the requested number of turns`` () =
    let current = { state 3 3 with TurnCount = 10; Messages = [ "Current" ] }
    let prior = { current with TurnCount = 6; Messages = [ "Earlier" ] }
    let promptSession =
        { State = current
          Modal = TimeShiftPrompt "1"
          History = [ prior ] }

    let transition = applyIntent inertServices Confirm promptSession

    match transition.NextSession with
    | Some nextSession ->
        nextSession.State.TurnCount |> should equal 6
        nextSession.Modal |> should equal NoModal
    | None ->
        failwith "Time shifter should not exit the game."

[<Fact>]
let ``load game intent uses the services boundary`` () =
    let loaded = { state 4 4 with TurnCount = 12; Messages = [ "Loaded state" ] }
    let transition = applyIntent (testServices loaded []) LoadGame (session (state 4 4))

    match transition.NextSession with
    | Some nextSession ->
        nextSession.State.TurnCount |> should equal 12
        nextSession.Modal |> should equal NoModal
        nextSession.State.Messages.Head |> should equal "Game loaded."
    | None ->
        failwith "Load should not exit the game."

[<Fact>]
let ``melee kill grants player the monster max hp boost`` () =
    let injuredPlayer = actor 0 "scavenger" 1 1 8 10 100 100 0 '@'
    let target = actor 1 "Cyberman" 2 1 1 3 50 70 0 'C'
    let game =
        { state 4 4 with
            Player = injuredPlayer
            Monsters = [ target ] }

    let next = update (Move(1, 0)) game

    next.Player.Hp |> should equal 10
    next.Monsters |> should be Empty
    next.Messages.Head |> should equal "You kill the Cyberman."

[<Fact>]
let ``ranged attack stops at walls and does not hit monsters beyond them`` () =
    let dungeon = map 5 1 Floor
    dungeon.Tiles[0, 2] <- Wall
    let target = actor 1 "Dalek" 4 0 5 5 50 70 0 'D'
    let game =
        { state 5 1 with
            Map = dungeon
            Player = actor 0 "scavenger" 0 0 10 10 100 100 0 '@'
            PlayerWeapon = weapon "Rusty raygun" 8 2 (Some 3)
            Monsters = [ target ] }

    let next = update (FireAt { X = 4; Y = 0 }) game

    next.Messages.Head |> should equal "Your Rusty raygun blasts the wall."
    next.PlayerWeapon.Ammo |> should equal (Some 2)
    next.Monsters.Head.Hp |> should equal 5

[<Fact>]
let ``ranged attack consumes ammo on hit`` () =
    let target = actor 1 "Dalek" 3 0 5 5 50 70 0 'D'
    let game =
        { state 5 1 with
            Player = actor 0 "scavenger" 0 0 10 10 100 100 0 '@'
            PlayerWeapon = weapon "Rusty raygun" 8 2 (Some 2)
            Monsters = [ target ] }

    let next = update (FireAt { X = 3; Y = 0 }) game

    next.PlayerWeapon.Ammo |> should equal (Some 1)
    next.Monsters.Head.Hp |> should equal 3
    next.Messages.Head |> should equal "You shoot the Dalek."

[<Fact>]
let ``monster speed two hundred attacks twice in one turn when adjacent`` () =
    let fastMonster = actor 1 "Dalek" 2 1 5 5 200 100 0 'D'
    let game =
        { state 4 3 with
            Player = actor 0 "scavenger" 1 1 10 10 100 100 0 '@'
            Monsters = [ fastMonster ] }

    let next = runMonsterTurn game

    next.Player.Hp |> should equal 8
    next.Messages |> List.filter (fun message -> message = "The Dalek hits you.") |> List.length |> should equal 2

[<Fact>]
let ``monster speed fifty acts every other turn`` () =
    let slowMonster = actor 1 "Cyberman" 2 1 5 5 50 100 0 'C'
    let game =
        { state 4 3 with
            Player = actor 0 "scavenger" 1 1 10 10 100 100 0 '@'
            Monsters = [ slowMonster ] }

    let afterOneTurn = runMonsterTurn game
    let afterTwoTurns = runMonsterTurn afterOneTurn

    afterOneTurn.Player.Hp |> should equal 10
    afterOneTurn.Monsters.Head.Energy |> should equal 50
    afterTwoTurns.Player.Hp |> should equal 9

[<Fact>]
let ``save game failure reports an error message`` () =
    let transition = applyIntent failingSaveServices SaveGame (session (state 3 3))

    match transition.NextSession with
    | Some nextSession ->
        nextSession.State.Messages.Head.StartsWith("Save failed: ") |> should equal true
        nextSession.Modal |> should equal NoModal
    | None ->
        failwith "Save should not exit the game."

[<Fact>]
let ``load game failure reports an error message`` () =
    let transition = applyIntent failingLoadServices LoadGame (session (state 3 3))

    match transition.NextSession with
    | Some nextSession ->
        nextSession.State.Messages.Head.StartsWith("Load failed: ") |> should equal true
        nextSession.Modal |> should equal NoModal
    | None ->
        failwith "Load should not exit the game."

[<Fact>]
let ``target mode confirm out of range does not fire`` () =
    let distantMonster = actor 1 "Dalek" 8 1 5 5 50 70 0 'D'
    let targetSession =
        { State =
            { state 10 3 with
                Player = actor 0 "scavenger" 1 1 10 10 100 100 0 '@'
                PlayerWeapon = weapon "Rusty raygun" 3 2 None
                Monsters = [ distantMonster ] }
          Modal = TargetMode { X = 8; Y = 1 }
          History = [] }

    let transition = applyIntent inertServices Confirm targetSession

    match transition.NextSession with
    | Some nextSession ->
        nextSession.State.Monsters.Head.Hp |> should equal 5
        nextSession.State.Messages.Head |> should equal "Rusty raygun cannot reach that far."
        nextSession.Modal |> should equal NoModal
    | None ->
        failwith "Target confirmation should not exit the game."

[<Fact>]
let ``time shifter prompt edits digits and cancel closes it`` () =
    let promptSession =
        { State = state 3 3
          Modal = TimeShiftPrompt ""
          History = [] }

    let afterOne =
        applyIntent inertServices (EnterDigit 1) promptSession
        |> fun transition -> transition.NextSession.Value

    let afterTwo =
        applyIntent inertServices (EnterDigit 2) afterOne
        |> fun transition -> transition.NextSession.Value

    let afterErase =
        applyIntent inertServices EraseDigit afterTwo
        |> fun transition -> transition.NextSession.Value

    let afterCancel =
        applyIntent inertServices Cancel afterErase
        |> fun transition -> transition.NextSession.Value

    afterOne.Modal |> should equal (TimeShiftPrompt "1")
    afterTwo.Modal |> should equal (TimeShiftPrompt "12")
    afterErase.Modal |> should equal (TimeShiftPrompt "1")
    afterCancel.Modal |> should equal NoModal
