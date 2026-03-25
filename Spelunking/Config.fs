(* Loads application settings plus CSV and JSON game data files into typed records. *)
module Spelunk.Config

open System
open System.IO
open System.Text.Json
open SadConsole
open Spelunk.Dungeon

type DungeonSettings =
    { MapWidth: int
      MapHeight: int
      RoomAttempts: int
      MinRoomWidth: int
      MaxRoomWidth: int
      MinRoomHeight: int
      MaxRoomHeight: int
      MaxOverlappingRooms: int }

type UiSettings =
    { StatsWidth: int
      GapRows: int
      LogRows: int
      CameraMargin: int
      PanelGap: int
      SightRadius: int }

type CombatSettings =
    { TargetRange: int }

type SpawnSettings =
    { MonsterRoomDensity: float
      MaxMonsters: int }

type WindowSettings =
    { Fullscreen: bool
      BorderlessWindowedFullscreen: bool
      DefaultFontSize: string }

type MonsterTemplate =
    { Name: string
      MaxHp: int
      Glyph: string
      Frequency: int
      MinDepth: int
      MaxDepth: int
      Speed: int
      Strength: int }

type WeaponTemplate =
    { Name: string
      Range: int
      Damage: int
      Ammo: int option }

type AppSettings =
    { Dungeon: DungeonSettings
      Ui: UiSettings
      Combat: CombatSettings
      Spawn: SpawnSettings
      Window: WindowSettings }

let private options =
    let settings = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
    settings.ReadCommentHandling <- JsonCommentHandling.Skip
    settings

let private loadSettings () : AppSettings =
    let path = Path.Combine(AppContext.BaseDirectory, "appsettings.json")

    if not (File.Exists path) then
        invalidOp $"Missing configuration file: {path}"

    let json = File.ReadAllText path
    let loaded: AppSettings = JsonSerializer.Deserialize<AppSettings>(json, options)

    if obj.ReferenceEquals(loaded, null) then
        invalidOp $"Invalid configuration file: {path}"
    else
        loaded

let private loadedSettings : Lazy<AppSettings> = lazy (loadSettings ())

let private parseMonsterCsvLine (line: string) : MonsterTemplate =
    let columns = line.Split(',', StringSplitOptions.TrimEntries)

    if columns.Length <> 8 then
        invalidOp $"Invalid monster CSV row: '{line}'"

    let parseInt fieldName (value: string) =
        match Int32.TryParse value with
        | true, parsed -> parsed
        | false, _ -> invalidOp $"Invalid integer value '{value}' for monster field '{fieldName}'."

    { Name = columns[0]
      MaxHp = parseInt "MaxHp" columns[1]
      Glyph = columns[2]
      Frequency = parseInt "Frequency" columns[3]
      MinDepth = parseInt "MinDepth" columns[4]
      MaxDepth = parseInt "MaxDepth" columns[5]
      Speed = parseInt "Speed" columns[6]
      Strength = parseInt "Strength" columns[7] }

let private loadMonsters () : MonsterTemplate list =
    let path = Path.Combine(AppContext.BaseDirectory, "Data", "Monsters.csv")

    if not (File.Exists path) then
        invalidOp $"Missing monster data file: {path}"

    let lines =
        File.ReadAllLines path
        |> Array.toList
        |> List.map (fun line -> line.Trim())
        |> List.filter (fun line -> line <> "" && not (line.StartsWith("#")))

    match lines with
    | [] -> []
    | header :: rows ->
        let expectedHeader = "Name,MaxHp,Glyph,Frequency,MinDepth,MaxDepth,Speed,Strength"

        if not (header.Equals(expectedHeader, StringComparison.OrdinalIgnoreCase)) then
            invalidOp $"Invalid monster CSV header. Expected '{expectedHeader}'."

        rows |> List.map parseMonsterCsvLine

let private loadedMonsters : Lazy<MonsterTemplate list> = lazy (loadMonsters ())

let private parseWeaponCsvLine (line: string) : WeaponTemplate =
    let columns = line.Split(',', StringSplitOptions.TrimEntries)

    if columns.Length <> 4 then
        invalidOp $"Invalid weapon CSV row: '{line}'"

    let parseInt fieldName (value: string) =
        match Int32.TryParse value with
        | true, parsed -> parsed
        | false, _ -> invalidOp $"Invalid integer value '{value}' for weapon field '{fieldName}'."

    let parseAmmo (value: string) =
        if String.IsNullOrWhiteSpace value then
            None
        else
            Some(parseInt "Ammo" value)

    { Name = columns[0]
      Range = parseInt "Range" columns[1]
      Damage = parseInt "Damage" columns[2]
      Ammo = parseAmmo columns[3] }

let private loadWeapons () : WeaponTemplate list =
    let path = Path.Combine(AppContext.BaseDirectory, "Data", "Weapons.csv")

    if not (File.Exists path) then
        invalidOp $"Missing weapon data file: {path}"

    let lines =
        File.ReadAllLines path
        |> Array.toList
        |> List.map (fun line -> line.Trim())
        |> List.filter (fun line -> line <> "" && not (line.StartsWith("#")))

    match lines with
    | [] -> []
    | header :: rows ->
        let expectedHeader = "Name,Range,Damage,Ammo"

        if not (header.Equals(expectedHeader, StringComparison.OrdinalIgnoreCase)) then
            invalidOp $"Invalid weapon CSV header. Expected '{expectedHeader}'."

        rows |> List.map parseWeaponCsvLine

let private loadedWeapons : Lazy<WeaponTemplate list> = lazy (loadWeapons ())

let appSettings () : AppSettings = loadedSettings.Value

let dungeonConfig () : GeneratorConfig =
    let dungeon : DungeonSettings = (appSettings ()).Dungeon

    { MapWidth = dungeon.MapWidth
      MapHeight = dungeon.MapHeight
      RoomAttempts = dungeon.RoomAttempts
      MinRoomWidth = dungeon.MinRoomWidth
      MaxRoomWidth = dungeon.MaxRoomWidth
      MinRoomHeight = dungeon.MinRoomHeight
      MaxRoomHeight = dungeon.MaxRoomHeight
      MaxOverlappingRooms = dungeon.MaxOverlappingRooms }

let uiSettings () : UiSettings =
    (appSettings ()).Ui

let windowSettings () : WindowSettings =
    (appSettings ()).Window

let combatSettings () : CombatSettings =
    (appSettings ()).Combat

let spawnSettings () : SpawnSettings =
    (appSettings ()).Spawn

let defaultFontSize () : IFont.Sizes =
    match (windowSettings ()).DefaultFontSize.Trim().ToLowerInvariant() with
    | "quarter" -> IFont.Sizes.Quarter
    | "half" -> IFont.Sizes.Half
    | "one" -> IFont.Sizes.One
    | "two" -> IFont.Sizes.Two
    | "three" -> IFont.Sizes.Three
    | "four" -> IFont.Sizes.Four
    | value -> invalidOp $"Unsupported Window:DefaultFontSize value '{value}'."

let monsterTemplates () : MonsterTemplate list =
    loadedMonsters.Value

let defaultWeaponTemplate () : WeaponTemplate =
    match loadedWeapons.Value with
    | weapon :: _ ->
        weapon
    | [] ->
        invalidOp "Data/Weapons.csv must define at least one weapon."
