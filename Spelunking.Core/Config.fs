(* Loads application settings plus CSV and JSON game data files into typed records. *)
module Spelunk.Config

open System
open System.IO
open System.Text.Json
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

let private defaultSettings =
    { Dungeon =
        { MapWidth = 400
          MapHeight = 200
          RoomAttempts = 150
          MinRoomWidth = 4
          MaxRoomWidth = 24
          MinRoomHeight = 3
          MaxRoomHeight = 20
          MaxOverlappingRooms = 10 }
      Ui =
        { StatsWidth = 16
          GapRows = 1
          LogRows = 4
          CameraMargin = 10
          PanelGap = 2
          SightRadius = 12 }
      Combat = { TargetRange = 8 }
      Spawn = { MonsterRoomDensity = 2.0; MaxMonsters = 1200 }
      Speech = { Enabled = true; Rate = 0; Volume = 100 }
      Window =
        { Fullscreen = true
          BorderlessWindowedFullscreen = true
          FontPath = None
          TileFontPath = Some "Fonts/WhoTiles.font"
          TextFontSize = Some "Two"
          TileFontSize = Some "Three"
          DefaultFontSize = "Two" } }

let private defaultMonsterRows =
    [ "Name,MaxHp,GlyphCode,Frequency,MinWorld,MaxWorld,Speed,Strength,Accuracy,VisionRadius,MeleeWeapon,RangedWeapon,Behavior,OnDeathEffect,Unique,Description,SpeechCue"
      "Cyberman,30,67,70,1,3,50,70,90,80,Rusty Knife,Rusty Raygun,Chaser,,FALSE,Cybernetic infantry that marches forward without hesitation.,Delete. Delete."
      "Raston Warrior Robot,40,82,20,1,6,500,40,120,100,Light Saber,,Chaser,,FALSE,An impossibly fast killing machine.,None survive."
      "Dalek,50,68,100,1,10,50,70,110,100,Rusty Knife,Exterminator,Sniper,,FALSE,A hateful war machine sealed inside its armored casing.,Exterminate. exterminate"
      "Ice Warrior,50,73,50,1,7,50,70,90,80,Lirpa,Revolver,Bruiser,,FALSE,A reptilian armored soldier from Mars.,Hiss."
      "Yeti,50,89,40,1,4,50,70,70,60,Batleath,,Bruiser,,FALSE,A bulky robotic servant that lumbers straight at intruders.,Roar."
      "Silurian,50,83,50,1,5,80,50,80,80,Rusty Knife,Revolver,Chaser,,FALSE,A calculating reptile humanoid from Earth's distant past.,Observe."
      "Sontaran,80,79,30,1,5,30,100,100,70,Lirpa,Rusty Raygun,Bruiser,,FALSE,A clone-bred soldier built entirely for war.,Sontar-Ha."
      "Auton,40,65,50,1,4,60,60,80,70,Rusty Knife,,Chaser,,FALSE,A faceless plastic killer animated by the Nestene Consciousness.,You will be like us."
      "Zygon,60,90,35,2,6,50,70,90,80,Rusty Knife,Bullwhip,Chaser,,FALSE,A shape-changing reptilian infiltrator from the lost world of Zygor.,This world can be ours."
      "Judoon,90,74,25,3,8,40,110,100,90,Batleath,Revolver,Bruiser,,FALSE,A rhinoceros-headed space police trooper in heavy armor.,Judoon platoon upon the moon."
      "Sea Devil,60,86,30,2,6,70,60,85,85,Rusty Knife,Revolver,Chaser,,FALSE,An amphibious reptile warrior risen from the oceans.,Return to the sea."
      "Ogron,90,71,25,2,6,40,120,60,60,Batleath,,Bruiser,,FALSE,A brutal alien mercenary used as muscle by more cunning masters.,Grrr."
      "Draconian,70,75,20,3,7,50,90,95,90,Lirpa,Revolver,Bruiser,,FALSE,A proud reptilian noble from the Draconian Empire.,Honor demands obedience."
      "Weeping Angel,70,87,15,4,10,200,130,120,110,Light Saber,,Chaser,,FALSE,A quantum-locked predator that closes distance in impossible bursts.,Do not blink."
      "Slitheen,100,72,15,4,9,40,140,70,75,Batleath,,Bruiser,,FALSE,A massive hunter wearing a stolen human disguise.,Feed me."
      "Sycorax,80,88,20,3,7,50,100,95,85,Lirpa,Bullwhip,Bruiser,,FALSE,An axe-bearing scavenger warlord from the stars.,Blood control."
      "Mire,110,77,12,5,10,40,150,100,90,Light Saber,Exterminator,Bruiser,,FALSE,A heavily armored war machine built for conquest.,You are beneath us."
      "Macra,120,81,10,5,10,30,160,70,65,Batleath,,Bruiser,,FALSE,A giant crab-like horror lurking in the dark passages.,There is no such thing as Macra."
      "Silence,70,78,18,4,9,80,80,110,95,Rusty Knife,Revolver,Sniper,,FALSE,A suit-clad confessional predator you forget the instant you look away.,Silence will fall."
      "Master,10000,70,1,8,88,25,70,100,150,Light Saber,Sonic Grenade,Bruiser,,TRUE,The Master," ]

let private defaultWeaponRows =
    [ "Name,Range,Damage,Ammo"
      "Rusty Knife,1,10,"
      "Rusty Raygun,8,20,40"
      "Revolver,4,10,80"
      "Sonic Grenade,3,50,4"
      "Exterminator,6,30,100"
      "Light Saber,1,30,"
      "Lirpa,1,30,"
      "Batleath,1,30,"
      "Bullwhip,2,20,40" ]

let private options =
    let settings = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
    settings.ReadCommentHandling <- JsonCommentHandling.Skip
    settings

let private loadSettings () : AppSettings =
    let path = Path.Combine(AppContext.BaseDirectory, "appsettings.json")

    if not (File.Exists path) then
        defaultSettings
    else
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

    let lines =
        if File.Exists path then
            File.ReadAllLines path |> Array.toList
        else
            defaultMonsterRows
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

    let lines =
        if File.Exists path then
            File.ReadAllLines path |> Array.toList
        else
            defaultWeaponRows
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

let configuredFontSizeName configured =
    let window = windowSettings ()

    configured
    |> Option.defaultValue window.DefaultFontSize

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
