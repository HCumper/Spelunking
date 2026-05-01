(* Persists and restores gameplay sessions as JSON without coupling the domain to file I/O. *)
module Spelunk.Save

open System
open System.IO
open System.Text.Json
open Spelunk.Model

module Dto =
    [<CLIMutable>]
    type SaveWeapon =
        { Name: string
          Range: int
          Damage: int
          Ammo: int option }

    [<CLIMutable>]
    type SaveActor =
        { Id: int
          Name: string
          X: int
          Y: int
          Hp: int
          MaxHp: int
          Speed: int
          Strength: int
          Energy: int
          MeleeWeapon: SaveWeapon
          RangedWeapon: SaveWeapon
          Glyph: JsonElement
          SpeechCue: string option
          Incarnation: int
          RegenerationsRemaining: int }

    [<CLIMutable>]
    type SaveGameState =
        { World: int
          TurnCount: int
          MapWidth: int
          MapHeight: int
          TilesRle: string
          Player: SaveActor
          Monsters: SaveActor list
          VisibleTilesRle: string
          ExploredTilesRle: string
          Messages: string list }

    [<CLIMutable>]
    type SaveSession =
        { Version: int
          State: SaveGameState
          History: SaveGameState list }

let private options =
    JsonSerializerOptions(WriteIndented = true, PropertyNameCaseInsensitive = true)

let private savePath () =
    let dataDirectory = Path.Combine(AppContext.BaseDirectory, "Data")
    Directory.CreateDirectory(dataDirectory) |> ignore
    Path.Combine(dataDirectory, "savegame.json")

let private tileToChar tile =
    match tile with
    | Wall -> '#'
    | Floor -> '.'
    | Tardis -> 'T'

let private charToTile glyph =
    match glyph with
    | '#' -> Wall
    | '.' -> Floor
    | 'T' -> Tardis
    | other -> invalidOp $"Unsupported tile glyph '{other}' in save file."

let private weaponToSave (weapon: Weapon) : Dto.SaveWeapon =
    { Name = weapon.Name
      Range = weapon.Range
      Damage = weapon.Damage
      Ammo = weapon.Ammo }

let private weaponFromSave (weapon: Dto.SaveWeapon) : Weapon =
    { Name = weapon.Name
      Range = weapon.Range
      Damage = weapon.Damage
      Ammo = weapon.Ammo }

let private glyphToSave glyph =
    JsonSerializer.SerializeToElement(glyph, options)

let private glyphFromSave (glyph: JsonElement) =
    match glyph.ValueKind with
    | JsonValueKind.Number ->
        match glyph.TryGetInt32() with
        | true, parsed when parsed >= 0 -> parsed
        | true, parsed -> invalidOp $"Invalid negative actor glyph code '{parsed}' in save file."
        | false, _ -> invalidOp "Invalid actor glyph code in save file."
    | JsonValueKind.String ->
        match glyph.GetString() with
        | null
        | "" -> int '?'
        | value ->
            match Int32.TryParse value with
            | true, parsed when parsed >= 0 -> parsed
            | true, parsed -> invalidOp $"Invalid negative actor glyph code '{parsed}' in save file."
            | false, _ when value.Length = 1 -> int value[0]
            | false, _ -> invalidOp $"Invalid actor glyph value '{value}' in save file."
    | JsonValueKind.Undefined
    | JsonValueKind.Null -> int '?'
    | _ -> invalidOp "Invalid actor glyph value in save file."

let private actorToSave (actor: Actor) : Dto.SaveActor =
    { Id = actor.Id
      Name = actor.Name
      X = actor.Position.X
      Y = actor.Position.Y
      Hp = actor.Hp
      MaxHp = actor.MaxHp
      Speed = actor.Speed
      Strength = actor.Strength
      Energy = actor.Energy
      MeleeWeapon = weaponToSave actor.MeleeWeapon
      RangedWeapon = weaponToSave actor.RangedWeapon
      Glyph = glyphToSave actor.Glyph
      SpeechCue = actor.SpeechCue
      Incarnation = actor.Incarnation
      RegenerationsRemaining = actor.RegenerationsRemaining }

let private actorFromSave (actor: Dto.SaveActor) : Actor =
    { Id = actor.Id
      Name = actor.Name
      Position = { X = actor.X; Y = actor.Y }
      Hp = actor.Hp
      MaxHp = actor.MaxHp
      Speed = actor.Speed
      Strength = actor.Strength
      Energy = actor.Energy
      MeleeWeapon = weaponFromSave actor.MeleeWeapon
      RangedWeapon = weaponFromSave actor.RangedWeapon
      Glyph = glyphFromSave actor.Glyph
      SpeechCue = actor.SpeechCue
      Incarnation = actor.Incarnation
      RegenerationsRemaining = actor.RegenerationsRemaining }

let private encodeRuns (glyphs: char seq) : string =
    let folder (parts: string list, current: char option, count: int) glyph =
        match current with
        | Some active when active = glyph ->
            parts, current, count + 1
        | Some active ->
            (sprintf "%d:%c" count active) :: parts, Some glyph, 1
        | None ->
            parts, Some glyph, 1

    let parts, current, count =
        glyphs |> Seq.fold folder ([], None, 0)

    let finalParts =
        match current with
        | Some active -> (sprintf "%d:%c" count active) :: parts
        | None -> parts

    finalParts |> List.rev |> String.concat ";"

let private decodeRuns (encoded: string) : char list =
    if String.IsNullOrWhiteSpace encoded then
        []
    else
        encoded.Split(';', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.collect (fun part ->
            let pieces = part.Split(':', 2)

            if pieces.Length <> 2 then
                invalidOp "Invalid run-length encoded save data."

            let count = Int32.Parse(pieces[0])
            let glyphs = pieces[1]

            if glyphs.Length <> 1 then
                invalidOp "Invalid run-length encoded glyph in save data."

            List.replicate count glyphs[0])

let private gridToRle (grid: bool[,]) : string =
    seq {
        for y in 0 .. Array2D.length1 grid - 1 do
            for x in 0 .. Array2D.length2 grid - 1 do
                yield if grid[y, x] then '1' else '0'
    }
    |> encodeRuns

let private rleToGrid (height: int) (width: int) (encoded: string) : bool[,] =
    let glyphs = decodeRuns encoded

    if glyphs.Length <> height * width then
        invalidOp "Run-length encoded grid size does not match save dimensions."

    Array2D.init height width (fun y x -> glyphs[(y * width) + x] = '1')

let private mapToRle (tiles: Map) : string =
    seq {
        for y in 0 .. tiles.Height - 1 do
            for x in 0 .. tiles.Width - 1 do
                yield tileToChar tiles.Tiles[y, x]
    }
    |> encodeRuns

let private mapFromRle (width: int) (height: int) (encoded: string) : Map =
    let glyphs = decodeRuns encoded

    if glyphs.Length <> height * width then
        invalidOp "Run-length encoded map size does not match save dimensions."

    let tiles =
        Array2D.init height width (fun y x ->
            glyphs[(y * width) + x] |> charToTile)

    { Width = width
      Height = height
      Tiles = tiles }

let private stateToSave (state: GameState) : Dto.SaveGameState =
    { World = state.World
      TurnCount = state.TurnCount
      MapWidth = state.Map.Width
      MapHeight = state.Map.Height
      TilesRle = mapToRle state.Map
      Player = actorToSave state.Player
      Monsters = state.Monsters |> List.map actorToSave
      VisibleTilesRle = gridToRle state.VisibleTiles
      ExploredTilesRle = gridToRle state.ExploredTiles
      Messages = state.Messages }

let private stateFromSave (state: Dto.SaveGameState) : GameState =
    { World = state.World
      TurnCount = state.TurnCount
      Map = mapFromRle state.MapWidth state.MapHeight state.TilesRle
      Player = actorFromSave state.Player
      Monsters = state.Monsters |> List.map actorFromSave
      VisibleTiles = rleToGrid state.MapHeight state.MapWidth state.VisibleTilesRle
      ExploredTiles = rleToGrid state.MapHeight state.MapWidth state.ExploredTilesRle
      Messages = state.Messages }

let serializeSession (state: GameState) (history: GameState list) : string =
    let payload : Dto.SaveSession =
        { Version = 3
          State = stateToSave state
          History = history |> List.map stateToSave }

    JsonSerializer.Serialize(payload, options)

let deserializeSession (json: string) : (GameState * GameState list) option =
    let loaded: Dto.SaveSession = JsonSerializer.Deserialize<Dto.SaveSession>(json, options)

    if obj.ReferenceEquals(loaded, null) then
        None
    else
        Some(stateFromSave loaded.State, loaded.History |> List.map stateFromSave)

let saveGame (state: GameState) (history: GameState list) : unit =
    let json = serializeSession state history
    File.WriteAllText(savePath (), json)

let tryLoadGame () : (GameState * GameState list) option =
    let path = savePath ()

    if not (File.Exists path) then
        None
    else
        let json = File.ReadAllText path
        deserializeSession json
