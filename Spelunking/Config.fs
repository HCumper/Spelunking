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

type SpeechSettings =
    { Enabled: bool
      Rate: int
      Volume: int }

type WindowSettings =
    { Fullscreen: bool
      BorderlessWindowedFullscreen: bool
      FontPath: string option
      TileFontPath: string option
      TextFontSize: string option
      TileFontSize: string option
      DefaultFontSize: string }

type MonsterTemplate =
    { Name: string
      MaxHp: int
      Glyph: int
      Frequency: int
      MinWorld: int
      MaxWorld: int
      Speed: int
      Strength: int
      Accuracy: int
      VisionRadius: int
      MeleeWeapon: string option
      RangedWeapon: string option
      Behavior: string
      OnDeathEffect: string option
      Unique: bool
      Description: string
      SpeechCue: string option }

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
      Speech: SpeechSettings
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

    if columns.Length <> 17 then
        invalidOp $"Invalid monster CSV row: '{line}'"

    let parseInt fieldName (value: string) =
        match Int32.TryParse value with
        | true, parsed -> parsed
        | false, _ -> invalidOp $"Invalid integer value '{value}' for monster field '{fieldName}'."

    let parseGlyph (value: string) =
        match Int32.TryParse value with
        | true, parsed when parsed >= 0 -> parsed
        | true, parsed -> invalidOp $"Invalid negative glyph code '{parsed}' for monster field 'GlyphCode'."
        | false, _ when not (String.IsNullOrWhiteSpace value) && value.Length = 1 -> int value[0]
        | false, _ -> invalidOp $"Invalid glyph value '{value}' for monster field 'GlyphCode'. Use a non-negative font glyph index or one character."

    let parseOptionalString (value: string) =
        if String.IsNullOrWhiteSpace value then None else Some value

    let parseBool fieldName (value: string) =
        match value.Trim().ToLowerInvariant() with
        | ""
        | "false"
        | "no"
        | "n"
        | "0" -> false
        | "true"
        | "yes"
        | "y"
        | "1" -> true
        | _ -> invalidOp $"Invalid boolean value '{value}' for monster field '{fieldName}'."

    { Name = columns[0]
      MaxHp = parseInt "MaxHp" columns[1]
      Glyph = parseGlyph columns[2]
      Frequency = parseInt "Frequency" columns[3]
      MinWorld = parseInt "MinWorld" columns[4]
      MaxWorld = parseInt "MaxWorld" columns[5]
      Speed = parseInt "Speed" columns[6]
      Strength = parseInt "Strength" columns[7]
      Accuracy = parseInt "Accuracy" columns[8]
      VisionRadius = parseInt "VisionRadius" columns[9]
      MeleeWeapon = parseOptionalString columns[10]
      RangedWeapon = parseOptionalString columns[11]
      Behavior = columns[12]
      OnDeathEffect = parseOptionalString columns[13]
      Unique = parseBool "Unique" columns[14]
      Description = columns[15]
      SpeechCue = parseOptionalString columns[16] }

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
        let expectedHeaders =
            [ "Name,MaxHp,GlyphCode,Frequency,MinWorld,MaxWorld,Speed,Strength,Accuracy,VisionRadius,MeleeWeapon,RangedWeapon,Behavior,OnDeathEffect,Unique,Description,SpeechCue"
              "Name,MaxHp,Glyph,Frequency,MinWorld,MaxWorld,Speed,Strength,Accuracy,VisionRadius,MeleeWeapon,RangedWeapon,Behavior,OnDeathEffect,Unique,Description,SpeechCue" ]

        if not (expectedHeaders |> List.exists (fun expected -> header.Equals(expected, StringComparison.OrdinalIgnoreCase))) then
            invalidOp $"Invalid monster CSV header. Expected '{expectedHeaders.Head}'."

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

let private resolveConfiguredPath (path: string option) : string option =
    path
    |> Option.bind (fun path ->
        if String.IsNullOrWhiteSpace path then
            None
        else
            let trimmed = path.Trim()

            if Path.IsPathRooted trimmed then
                Some trimmed
            else
                Some(Path.Combine(AppContext.BaseDirectory, trimmed)))

let defaultFontPath () : string option =
    None

let tileFontPath () : string option =
    let window = windowSettings ()

    match resolveConfiguredPath window.TileFontPath with
    | Some path -> Some path
    | None -> resolveConfiguredPath window.FontPath

let combatSettings () : CombatSettings =
    (appSettings ()).Combat

let spawnSettings () : SpawnSettings =
    (appSettings ()).Spawn

let speechSettings () : SpeechSettings =
    (appSettings ()).Speech

let private fontSizeFromString settingName (value: string) : IFont.Sizes =
    match value.Trim().ToLowerInvariant() with
    | "quarter" -> IFont.Sizes.Quarter
    | "half" -> IFont.Sizes.Half
    | "one" -> IFont.Sizes.One
    | "two" -> IFont.Sizes.Two
    | "three" -> IFont.Sizes.Three
    | "four" -> IFont.Sizes.Four
    | value -> invalidOp $"Unsupported Window:{settingName} value '{value}'."

let private configuredFontSize settingName configured =
    let window = windowSettings ()

    configured
    |> Option.defaultValue window.DefaultFontSize
    |> fontSizeFromString settingName

let defaultFontSize () : IFont.Sizes =
    configuredFontSize "TextFontSize" (windowSettings ()).TextFontSize

let tileFontSize () : IFont.Sizes =
    configuredFontSize "TileFontSize" (windowSettings ()).TileFontSize

let monsterTemplates () : MonsterTemplate list =
    loadedMonsters.Value

let weaponTemplateByName (name: string) : WeaponTemplate option =
    loadedWeapons.Value
    |> List.tryFind (fun weapon -> weapon.Name.Equals(name, StringComparison.OrdinalIgnoreCase))

let private defaultWeaponTemplateBy predicate errorMessage =
    match loadedWeapons.Value |> List.tryFind predicate with
    | Some weapon -> weapon
    | None -> invalidOp errorMessage

let defaultMeleeWeaponTemplate () : WeaponTemplate =
    defaultWeaponTemplateBy
        (fun weapon -> weapon.Range = 1)
        "Data/Weapons.csv must define at least one melee weapon with range 1."

let defaultRangedWeaponTemplate () : WeaponTemplate =
    defaultWeaponTemplateBy
        (fun weapon -> weapon.Range > 1)
        "Data/Weapons.csv must define at least one ranged weapon with range greater than 1."
