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
      Glyph: char }

type Map =
    { Width: int
      Height: int
      Tiles: Tile[,] }

type GameState =
    { Depth: int
      Map: Map
      Player: Actor
      Monsters: Actor list
      VisibleTiles: bool[,]
      ExploredTiles: bool[,]
      Messages: string list }

type Command =
    | Move of dx: int * dy: int
    | Wait
