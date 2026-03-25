(* Converts generated dungeon layouts into map tiles and exposes room helpers used by spawning and stairs placement. *)
module Spelunk.World

open Spelunk.Config
open Spelunk.Dungeon
open Spelunk.Model

let roomCenter (room: Room) =
    { X = room.Left + room.Width / 2
      Y = room.Top + room.Height / 2 }

let pointInRoom (room: Room) point =
    point.X >= room.Left
    && point.X < room.Left + room.Width
    && point.Y >= room.Top
    && point.Y < room.Top + room.Height

let private fallbackFloorPoint (layout: Layout) =
    seq {
        for y in 0 .. layout.Height - 1 do
            for x in 0 .. layout.Width - 1 do
                if layout.Floors[y, x] then
                    yield { X = x; Y = y }
    }
    |> Seq.tryHead
    |> Option.defaultValue { X = 1; Y = 1 }

let createDungeon () =
    let layout: Layout = generate (dungeonConfig ())
    let tiles = Array2D.create layout.Height layout.Width Wall

    // The generator exposes only a floor mask; world creation turns those cells into concrete tiles.
    for y in 0 .. layout.Height - 1 do
        for x in 0 .. layout.Width - 1 do
            if layout.Floors[y, x] then
                tiles[y, x] <- Floor

    let stairPoint =
        layout.Rooms
        |> List.tryLast
        |> Option.map roomCenter
        |> Option.defaultValue (fallbackFloorPoint layout)

    tiles[stairPoint.Y, stairPoint.X] <- StairsDown

    { Width = layout.Width
      Height = layout.Height
      Tiles = tiles },
    layout.Rooms
