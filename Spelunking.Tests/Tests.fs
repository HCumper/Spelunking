module Spelunking.Tests

open System
open System.IO
open FsUnit.Xunit
open Spelunk.Application
open Spelunk.Combat
open Spelunk.Config
open Spelunk.Model
open Spelunk.Output
open Spelunk.Services
open Spelunk.SessionActions
open Spelunk.SessionHistory
open Spelunk.Simulation
open Spelunk.Visibility
open Spelunk.World
open Xunit

let private weapon name range damage ammo : Weapon =
    { Spelunk.Model.Weapon.Name = name
      Range = range
      Damage = damage
      Ammo = ammo }

let private actor id name x y hp maxHp speed strength energy glyph : Actor =
    { Spelunk.Model.Actor.Id = id
      Name = name
      Position = { X = x; Y = y }
      Hp = hp
      MaxHp = maxHp
      Speed = speed
      Strength = strength
      Energy = energy
      MeleeWeapon = weapon "Rusty knife" 1 10 None
      RangedWeapon = weapon "Rusty raygun" 8 20 None
      Glyph = int glyph
      SpeechCue = None }

let private withWeapons (meleeWeapon: Weapon) (rangedWeapon: Weapon) (actor: Actor) : Actor =
    { actor with
        MeleeWeapon = meleeWeapon
        RangedWeapon = rangedWeapon }

let private withGlyphCode glyph (actor: Actor) : Actor =
    { actor with Glyph = glyph }

let private map width height defaultTile =
    { Width = width
      Height = height
      Tiles = Array2D.create height width defaultTile }

let private state width height =
    let dungeon = map width height Floor

    { World = 1
      TurnCount = 0
      Map = dungeon
      Player = actor 0 "scavenger" 1 1 100 100 100 100 0 '@'
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
      TryLoadGame = fun () -> Some(loadedState, loadedHistory)
      Speak = fun _ -> () }

let private inertServices =
    { SaveGame = fun _ -> ()
      TryLoadGame = fun () -> None
      Speak = fun _ -> () }

let private failingSaveServices : Services =
    { SaveGame = fun (_: GameState * GameState list) -> raise (Exception("disk full"))
      TryLoadGame = fun () -> None
      Speak = fun _ -> () }

let private failingLoadServices : Services =
    { SaveGame = fun _ -> ()
      TryLoadGame = fun () -> raise (Exception("corrupt file"))
      Speak = fun _ -> () }

let private saveFilePath () =
    Path.Combine(AppContext.BaseDirectory, "Data", "savegame.json")

let private assertStateEqual actual expected =
    actual.World |> should equal expected.World
    actual.TurnCount |> should equal expected.TurnCount
    actual.Map |> should equal expected.Map
    actual.Player |> should equal expected.Player
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

    damageFor weak |> should equal 10
    damageFor baseline |> should equal 10
    damageFor strong |> should equal 20
    damageFor elite |> should equal 20

[<Fact>]
let ``save and load round-trip game state and history`` () =
    let original =
        { state 4 4 with
            TurnCount = 7
            Player = actor 0 "scavenger" 2 1 80 100 100 100 0 '@'
            Monsters = [ actor 1 "Cyberman" 3 3 20 30 50 70 40 'C' |> withGlyphCode 260 ]
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

    Spelunk.Save.saveGame original [ prior ]

    match Spelunk.Save.tryLoadGame () with
    | Some (loadedState, loadedHistory) ->
        assertStateEqual loadedState original
        loadedHistory.Length |> should equal 1
        assertStateEqual loadedHistory.Head prior
    | None ->
        failwith "Expected save file to load."

[<Fact>]
let ``monster data loads numeric glyph codes`` () =
    let dalek = monsterTemplates () |> List.find (fun monster -> monster.Name = "Dalek")

    dalek.Glyph |> should equal 68

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
let ``run continues through a corridor until a branch appears`` () =
    let dungeon = map 5 5 Wall

    [ (1, 2); (2, 2); (3, 2); (4, 2); (4, 1) ]
    |> List.iter (fun (x, y) -> dungeon.Tiles[y, x] <- Floor)

    let startState =
        { state 5 5 with
            Map = dungeon
            Player = actor 0 "scavenger" 1 2 100 100 100 100 0 '@' }

    let startSession = session startState
    let transition = applyIntent inertServices (Run(1, 0)) startSession
    let nextSession = transition.NextSession.Value

    nextSession.State.Player.Position |> should equal { X = 3; Y = 2 }
    nextSession.History.Length |> should equal 2

[<Fact>]
let ``run stops when a monster becomes visible`` () =
    let dungeon = map 6 5 Wall

    [ (1, 2); (2, 2); (3, 2); (4, 2); (4, 1) ]
    |> List.iter (fun (x, y) -> dungeon.Tiles[y, x] <- Floor)

    let lurkingMonster = actor 1 "Dalek" 4 1 100 100 100 100 0 'D'

    let startState =
        { state 6 5 with
            Map = dungeon
            Player = actor 0 "scavenger" 1 2 100 100 100 100 0 '@'
            Monsters = [ lurkingMonster ] }
        |> computeVisibility

    startState.VisibleTiles[1, 4] |> should equal false

    let result = applyIntent inertServices (Run(1, 0)) (session startState)
    let nextSession = result.NextSession.Value

    nextSession.State.Player.Position |> should equal { X = 2; Y = 2 }
    nextSession.History.Length |> should equal 1

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
            Player = actor 0 "scavenger" 0 1 100 100 100 100 0 '@'
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
let ``look confirm advances to the next visible object and then closes`` () =
    let dungeon = map 6 3 Floor
    dungeon.Tiles[1, 5] <- Wall

    let lookoutMonster = actor 1 "Dalek" 3 1 50 50 50 70 0 'D'
    let lookState =
        { state 6 3 with
            Map = dungeon
            Player = actor 0 "scavenger" 1 1 100 100 100 100 0 '@'
            Monsters = [ lookoutMonster ] }
        |> computeVisibility

    let opened =
        applyIntent inertServices OpenLook (session lookState)
        |> fun transition -> transition.NextSession.Value

    let atMonster =
        applyIntent inertServices (MoveCursor(1, 0)) opened
        |> fun transition -> transition.NextSession.Value

    let atWall =
        applyIntent inertServices Confirm atMonster
        |> fun transition -> transition.NextSession.Value

    let closed =
        applyIntent inertServices Confirm atWall
        |> fun transition -> transition.NextSession.Value

    atMonster.Modal |> should equal (LookMode { X = 3; Y = 1 })
    atWall.Modal |> should equal (LookMode { X = 5; Y = 1 })
    closed.Modal |> should equal NoModal

[<Fact>]
let ``melee kill grants player the monster max hp boost`` () =
    let injuredPlayer = actor 0 "scavenger" 1 1 80 100 100 100 0 '@'
    let target = actor 1 "Cyberman" 2 1 10 30 50 70 0 'C'
    let game =
        { state 4 4 with
            Player = injuredPlayer
            Monsters = [ target ] }

    let next = Spelunk.Domain.update (Move(1, 0)) game

    next.Player.Hp |> should equal 95
    next.Monsters |> should be Empty
    next.Messages.Head |> should equal "You kill the Cyberman."

[<Fact>]
let ``ranged attack stops at walls and does not hit monsters beyond them`` () =
    let dungeon = map 5 1 Floor
    dungeon.Tiles[0, 2] <- Wall
    let target = actor 1 "Dalek" 4 0 50 50 50 70 0 'D'
    let game =
        { state 5 1 with
            Map = dungeon
            Player =
                actor 0 "scavenger" 0 0 100 100 100 100 0 '@'
                |> withWeapons (weapon "Rusty knife" 1 10 None) (weapon "Rusty raygun" 8 20 (Some 3))
            Monsters = [ target ] }

    let next = update (FireAt { X = 4; Y = 0 }) game

    next.Messages.Head |> should equal "Your Rusty raygun blasts the wall."
    next.Player.RangedWeapon.Ammo |> should equal (Some 2)
    next.Monsters.Head.Hp |> should equal 50

[<Fact>]
let ``ranged attack consumes ammo on hit`` () =
    let target = actor 1 "Dalek" 3 0 70 70 50 70 0 'D'
    let game =
        { state 5 1 with
            Player =
                actor 0 "scavenger" 0 0 100 100 100 100 0 '@'
                |> withWeapons (weapon "Rusty knife" 1 10 None) (weapon "Rusty raygun" 8 20 (Some 2))
            Monsters = [ target ] }

    let next = update (FireAt { X = 3; Y = 0 }) game

    next.Player.RangedWeapon.Ammo |> should equal (Some 1)
    next.Monsters.Head.Hp |> should equal 50
    next.Messages.Head |> should equal "You shoot the Dalek."

[<Fact>]
let ``player ranged attack emits a projectile path to the first blocker`` () =
    let dungeon = map 5 1 Floor
    let target =
        actor 1 "Dalek" 3 0 50 50 50 70 0 'D'

    let startSession =
        { State =
            { state 5 1 with
                Player =
                    actor 0 "scavenger" 0 0 100 100 100 100 0 '@'
                    |> withWeapons (weapon "Rusty knife" 1 10 None) (weapon "Rusty raygun" 8 20 (Some 2))
                Monsters = [ target ] }
          Modal = NoModal
          History = [] }

    let transition = applyIntent inertServices (Act(FireAt { X = 4; Y = 0 })) startSession

    transition.Events
    |> List.exists (function
        | AnimateProjectile path -> path = [ { X = 1; Y = 0 }; { X = 2; Y = 0 }; { X = 3; Y = 0 } ]
        | _ -> false)
    |> should equal true

[<Fact>]
let ``player ranged attack animation continues to range limit on a miss`` () =
    let startSession =
        { State =
            { state 8 1 with
                Player =
                    actor 0 "scavenger" 0 0 100 100 100 100 0 '@'
                    |> withWeapons (weapon "Rusty knife" 1 10 None) (weapon "Rusty raygun" 5 20 (Some 2)) }
          Modal = NoModal
          History = [] }

    let transition = applyIntent inertServices (Act(FireAt { X = 2; Y = 0 })) startSession

    transition.Events
    |> List.exists (function
        | AnimateProjectile path ->
            path = [ { X = 1; Y = 0 }; { X = 2; Y = 0 }; { X = 3; Y = 0 }; { X = 4; Y = 0 }; { X = 5; Y = 0 } ]
        | _ -> false)
    |> should equal true

[<Fact>]
let ``ranged attack that cannot reach a monster reports falls short`` () =
    let target = actor 1 "Dalek" 6 0 50 50 50 70 0 'D'
    let game =
        { state 8 1 with
            Player =
                actor 0 "scavenger" 0 0 100 100 100 100 0 '@'
                |> withWeapons (weapon "Rusty knife" 1 10 None) (weapon "Rusty raygun" 5 20 (Some 2))
            Monsters = [ target ] }

    let next = update (FireAt { X = 5; Y = 0 }) game

    next.Messages.Head |> should equal "Your Rusty raygun falls short of the Dalek."

[<Fact>]
let ``moving onto the tardis generates the next world`` () =
    let dungeon = map 3 3 Floor
    dungeon.Tiles[1, 2] <- Tardis

    let game =
        { state 3 3 with
            World = 4
            Map = dungeon
            Player = actor 0 "scavenger" 1 1 75 100 100 100 0 '@'
            Monsters = [ actor 1 "Cyberman" 0 0 30 30 50 70 0 'C' ] }

    let next = Spelunk.Domain.update (Move(1, 0)) game

    next.World |> should equal 5
    next.Player.Hp |> should equal 75
    next.Player.Position |> should not' (equal { X = 2; Y = 1 })
    next.Messages.Head |> should equal "The Tardis hurtles through time and space and re-materialized."

[<Fact>]
let ``createDungeon places exactly one tardis on a walkable tile`` () =
    let map, _ = createDungeon ()

    let tardisTiles =
        [ for y in 0 .. map.Height - 1 do
              for x in 0 .. map.Width - 1 do
                  if map.Tiles[y, x] = Tardis then
                      yield { X = x; Y = y } ]

    tardisTiles.Length |> should equal 1
    let tardis = tardisTiles.Head
    (tardis.X >= 0 && tardis.X < map.Width) |> should equal true
    (tardis.Y >= 0 && tardis.Y < map.Height) |> should equal true

[<Fact>]
let ``monster speed two hundred attacks twice in one turn when adjacent`` () =
    let fastMonster = actor 1 "Dalek" 2 1 5 5 200 100 0 'D'
    let game =
        { state 4 3 with
            Player = actor 0 "scavenger" 1 1 100 100 100 100 0 '@'
            Monsters = [ fastMonster ] }

    let next = runMonsterTurn game

    next.Player.Hp |> should equal 80
    next.Messages |> List.filter (fun message -> message = "The Dalek hits you.") |> List.length |> should equal 2

[<Fact>]
let ``monster speed fifty acts every other turn`` () =
    let slowMonster = actor 1 "Cyberman" 2 1 5 5 50 100 0 'C'
    let game =
        { state 4 3 with
            Player = actor 0 "scavenger" 1 1 100 100 100 100 0 '@'
            Monsters = [ slowMonster ] }

    let afterOneTurn = runMonsterTurn game
    let afterTwoTurns = runMonsterTurn afterOneTurn

    afterOneTurn.Player.Hp |> should equal 100
    afterOneTurn.Monsters.Head.Energy |> should equal 50
    afterTwoTurns.Player.Hp |> should equal 90

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
let ``pressing quit while dead exits immediately`` () =
    let deadSession =
        session
            { state 3 3 with
                Player = actor 0 "scavenger" 1 1 0 100 100 100 0 '@' }

    let transition = applyIntent inertServices Quit deadSession

    transition.NextSession |> should equal None

[<Fact>]
let ``target mode confirm out of range does not fire`` () =
    let distantMonster = actor 1 "Dalek" 8 1 50 50 50 70 0 'D'
    let targetSession =
        { State =
            { state 10 3 with
                Player =
                    actor 0 "scavenger" 1 1 100 100 100 100 0 '@'
                    |> withWeapons (weapon "Rusty knife" 1 10 None) (weapon "Rusty raygun" 3 20 None)
                Monsters = [ distantMonster ] }
          Modal = TargetMode { X = 8; Y = 1 }
          History = [] }

    let transition = applyIntent inertServices Confirm targetSession

    match transition.NextSession with
    | Some nextSession ->
        nextSession.State.Monsters.Head.Hp |> should equal 50
        nextSession.State.Messages.Head |> should equal "Rusty raygun cannot reach that far."
        nextSession.Modal |> should equal NoModal
    | None ->
        failwith "Target confirmation should not exit the game."

[<Fact>]
let ``target mode directional input fires immediately and closes`` () =
    let dalek = actor 1 "Dalek" 4 1 50 50 50 70 0 'D'
    let targetSession =
        { State =
            { state 8 3 with
                Player =
                    actor 0 "scavenger" 1 1 100 100 100 100 0 '@'
                    |> withWeapons (weapon "Rusty knife" 1 10 None) (weapon "Rusty raygun" 5 20 (Some 2))
                Monsters = [ dalek ] }
          Modal = TargetMode { X = 1; Y = 1 }
          History = [] }

    let transition = applyIntent inertServices (MoveCursor(1, 0)) targetSession

    match transition.NextSession with
    | Some nextSession ->
        nextSession.Modal |> should equal NoModal
        nextSession.State.Monsters.Head.Hp |> should equal 30
        transition.Events
        |> List.exists (function
            | AnimateProjectile path -> path = [ { X = 2; Y = 1 }; { X = 3; Y = 1 }; { X = 4; Y = 1 } ]
            | _ -> false)
        |> should equal true
    | None ->
        failwith "Directional fire should not exit the game."

[<Fact>]
let ``monster ranged fire can hit another monster standing in the way`` () =
    let dalek =
        actor 1 "Dalek" 0 0 50 50 100 100 0 'D'
        |> withWeapons
            (weapon "Plunger" 1 10 None)
            (weapon "Exterminator" 6 30 (Some 2))

    let cyberman =
        actor 2 "Cyberman" 1 0 30 30 100 100 0 'C'
        |> withWeapons (weapon "Rusty knife" 1 10 None) (weapon "Rusty raygun" 8 20 None)

    let game =
        { state 5 1 with
            Player = actor 0 "scavenger" 4 0 100 100 100 100 0 '@'
            Monsters = [ dalek; cyberman ] }

    let next = runMonsterTurn game
    let dalekAfter = next.Monsters |> List.find (fun monster -> monster.Id = 1)

    next.Monsters |> List.exists (fun monster -> monster.Id = 2) |> should equal false
    dalekAfter.RangedWeapon.Ammo |> should equal (Some 1)
    next.Player.Hp |> should equal 100
    next.Messages.Head |> should equal "The Dalek shoots at you and blasts the Cyberman apart."

[<Fact>]
let ``player and monster ranged shots animate and share one message line for the turn`` () =
    let dalek =
        actor 1 "Dalek" 3 0 50 50 100 100 0 'D'
        |> withWeapons
            (weapon "Plunger" 1 10 None)
            (weapon "Exterminator" 6 20 (Some 2))

    let startSession =
        { State =
            { state 8 1 with
                Player =
                    actor 0 "scavenger" 0 0 100 100 100 100 0 '@'
                    |> withWeapons (weapon "Rusty knife" 1 10 None) (weapon "Rusty raygun" 6 20 (Some 2))
                Monsters = [ dalek ] }
          Modal = NoModal
          History = [] }

    let transition = applyIntent inertServices (Act(FireAt { X = 6; Y = 0 })) startSession
    let nextSession = transition.NextSession.Value

    nextSession.State.Messages.Head |> should equal "You shoot the Dalek. The Dalek shoots you."
    nextSession.State.Player.Hp |> should equal 80
    nextSession.State.Monsters.Head.Hp |> should equal 30
    (transition.Events |> List.filter (function | AnimateProjectile _ -> true | _ -> false) |> List.length)
    |> should equal 2

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
