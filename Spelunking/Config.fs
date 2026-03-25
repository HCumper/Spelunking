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

type MonsterCollection =
    { Monsters: MonsterTemplate list }

type WeaponCollection =
    { Weapons: WeaponTemplate list }

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

let private loadMonsters () : MonsterCollection =
    let path = Path.Combine(AppContext.BaseDirectory, "Data", "Monsters.json")

    if not (File.Exists path) then
        invalidOp $"Missing monster data file: {path}"

    let json = File.ReadAllText path
    let loaded: MonsterCollection = JsonSerializer.Deserialize<MonsterCollection>(json, options)

    if obj.ReferenceEquals(loaded, null) then
        invalidOp $"Invalid monster data file: {path}"
    else
        loaded

let private loadedMonsters : Lazy<MonsterCollection> = lazy (loadMonsters ())

let private loadWeapons () : WeaponCollection =
    let path = Path.Combine(AppContext.BaseDirectory, "Data", "Weapons.json")

    if not (File.Exists path) then
        invalidOp $"Missing weapon data file: {path}"

    let json = File.ReadAllText path
    let loaded: WeaponCollection = JsonSerializer.Deserialize<WeaponCollection>(json, options)

    if obj.ReferenceEquals(loaded, null) then
        invalidOp $"Invalid weapon data file: {path}"
    else
        loaded

let private loadedWeapons : Lazy<WeaponCollection> = lazy (loadWeapons ())

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
    (loadedMonsters.Value).Monsters

let defaultWeaponTemplate () : WeaponTemplate =
    match (loadedWeapons.Value).Weapons with
    | weapon :: _ ->
        weapon
    | [] ->
        invalidOp "Data/Weapons.json must define at least one weapon."
