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
    { TargetRange: int
      MonsterAttackDamage: int }

type WindowSettings =
    { Fullscreen: bool
      BorderlessWindowedFullscreen: bool
      DefaultFontSize: string }

type MonsterTemplate =
    { Id: int
      Name: string
      MaxHp: int
      Glyph: string }

type AppSettings =
    { Dungeon: DungeonSettings
      Ui: UiSettings
      Combat: CombatSettings
      Window: WindowSettings }

type MonsterCollection =
    { Monsters: MonsterTemplate list }

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
    let path = Path.Combine(AppContext.BaseDirectory, "Monsters.json")

    if not (File.Exists path) then
        invalidOp $"Missing monster data file: {path}"

    let json = File.ReadAllText path
    let loaded: MonsterCollection = JsonSerializer.Deserialize<MonsterCollection>(json, options)

    if obj.ReferenceEquals(loaded, null) then
        invalidOp $"Invalid monster data file: {path}"
    else
        loaded

let private loadedMonsters : Lazy<MonsterCollection> = lazy (loadMonsters ())

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
