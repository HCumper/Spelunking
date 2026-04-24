(* Character-cell appearance rules for the SadConsole renderer. *)
module Spelunk.Appearance

open SadRogue.Primitives
open Spelunk.Model

type CellAppearance =
    { Glyph: int
      Foreground: Color
      Background: Color }

module Glyphs =
    let Wall = 16
    let Floor = 0
    let Tardis = int 'T'
    let Player = 64
    let Projectile = int '*'

let tileAppearance tile explored : CellAppearance =
    match tile, explored with
    | Wall, false ->
        { Glyph = Glyphs.Wall
          Foreground = Color.Gray
          Background = Color.Black }
    | Wall, true ->
        { Glyph = Glyphs.Wall
          Foreground = Color.DarkGray
          Background = Color.Black }
    | Floor, false ->
        { Glyph = Glyphs.Floor
          Foreground = Color.DarkSlateGray
          Background = Color.Black }
    | Floor, true ->
        { Glyph = Glyphs.Floor
          Foreground = Color.DimGray
          Background = Color.Black }
    | Tardis, false ->
        { Glyph = Glyphs.Tardis
          Foreground = Color.DeepSkyBlue
          Background = Color.Black }
    | Tardis, true ->
        { Glyph = Glyphs.Tardis
          Foreground = Color.SteelBlue
          Background = Color.Black }

let playerAppearance : CellAppearance =
    { Glyph = Glyphs.Player
      Foreground = Color.White
      Background = Color.Black }

let monsterAppearance (monster: Actor) : CellAppearance =
    let foreground =
        match monster.Glyph with
        | glyph when glyph = int 'D' -> Color.LightSteelBlue
        | glyph when glyph = int 'C' -> Color.LightGray
        | glyph when glyph = int 'W' -> Color.White
        | glyph when glyph = int 'M' -> Color.DarkRed
        | glyph when glyph = int 'N' -> Color.DarkGray
        | glyph when glyph = int 'R' -> Color.Gold
        | _ -> Color.IndianRed

    { Glyph = monster.Glyph
      Foreground = foreground
      Background = Color.Black }

let projectileAppearance : CellAppearance =
    { Glyph = Glyphs.Projectile
      Foreground = Color.Gold
      Background = Color.Black }
