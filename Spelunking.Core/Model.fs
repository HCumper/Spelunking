(* Core domain types shared by generation, simulation, application state, and UI rendering. *)
module Spelunk.Model

type Position = { X: int; Y: int }

type Tile =
    | Wall
    | Floor
    | Tardis

type Weapon =
    { Name: string
      Range: int
      Damage: int
      Ammo: int option }

let playerGlyphForIncarnation incarnation =
    let glyphs =
        [ 64
          int '&'
          int '9'
          int '8'
          int '6'
          int 'Q'
          int 'P'
          int 'B'
          int 'R'
          int 'M'
          int 'W'
          int 'K'
          int 'A' ]

    glyphs[max 0 (min (glyphs.Length - 1) incarnation)]

type Actor =
    { Id: int
      Name: string
      Position: Position
      Hp: int
      MaxHp: int
      Speed: int
      Strength: int
      Energy: int
      MeleeWeapon: Weapon
      RangedWeapon: Weapon
      Glyph: int
      SpeechCue: string option
      Incarnation: int
      RegenerationsRemaining: int }

type Map =
    { Width: int
      Height: int
      Tiles: Tile[,] }

type GameState =
    { World: int
      TurnCount: int
      Map: Map
      Player: Actor
      Monsters: Actor list
      VisibleTiles: bool[,]
      ExploredTiles: bool[,]
      Messages: string list }

type Command =
    | Move of dx: int * dy: int
    | FireAt of Position
    | Wait
