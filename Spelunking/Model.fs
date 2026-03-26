(* Core domain types shared by generation, simulation, application state, and UI rendering. *)
module Spelunk.Model

type Position = { X: int; Y: int }

type Tile =
    | Wall
    | Floor
    | StairsDown

type Actor =
    { Id: int
      Name: string
      Position: Position
      Hp: int
      MaxHp: int
      Speed: int
      Strength: int
      Energy: int
      Glyph: char
      SpeechCue: string option }

type Weapon =
    { Name: string
      Range: int
      Damage: int
      Ammo: int option }

type Map =
    { Width: int
      Height: int
      Tiles: Tile[,] }

type GameState =
    { Depth: int
      TurnCount: int
      Map: Map
      Player: Actor
      PlayerWeapon: Weapon
      Monsters: Actor list
      VisibleTiles: bool[,]
      ExploredTiles: bool[,]
      Messages: string list }

type Command =
    | Move of dx: int * dy: int
    | FireAt of Position
    | Wait
