(* Computes field-of-view and persistent exploration memory for the player. *)
module Spelunk.Visibility

open Spelunk.Config
open Spelunk.Model

let copyGrid (grid: bool[,]) =
    Array2D.init (Array2D.length1 grid) (Array2D.length2 grid) (fun y x -> grid[y, x])

let private tileAt map point =
    map.Tiles[point.Y, point.X]

let private blocksSight tile =
    match tile with
    | Wall -> true
    | Floor
    | StairsDown -> false

let private positionsOnLine startPoint endPoint =
    let dx = abs (endPoint.X - startPoint.X)
    let dy = abs (endPoint.Y - startPoint.Y)
    let stepX = compare endPoint.X startPoint.X
    let stepY = compare endPoint.Y startPoint.Y

    let rec walk x y err acc =
        let nextAcc = { X = x; Y = y } :: acc

        if x = endPoint.X && y = endPoint.Y then
            List.rev nextAcc
        else
            let doubleError = err * 2
            let nextX, nextErr =
                if doubleError > -dy then
                    x + stepX, err - dy
                else
                    x, err

            let nextY, finalErr =
                if doubleError < dx then
                    y + stepY, nextErr + dx
                else
                    y, nextErr

            walk nextX nextY finalErr nextAcc

    walk startPoint.X startPoint.Y (dx - dy) []

let private hasLineOfSight map startPoint endPoint =
    // Intermediate walls block vision, but the destination tile itself may still be seen.
    positionsOnLine startPoint endPoint
    |> List.skip 1
    |> List.takeWhile (fun point -> point <> endPoint)
    |> List.forall (fun point -> not (blocksSight (tileAt map point)))

let computeVisibility state =
    let radius = (uiSettings ()).SightRadius
    let visible = Array2D.create state.Map.Height state.Map.Width false
    let explored = copyGrid state.ExploredTiles
    let origin = state.Player.Position

    // Visibility is evaluated inside a radius-limited square and then filtered by LOS.
    for y in max 0 (origin.Y - radius) .. min (state.Map.Height - 1) (origin.Y + radius) do
        for x in max 0 (origin.X - radius) .. min (state.Map.Width - 1) (origin.X + radius) do
            let point = { X = x; Y = y }
            let dx = x - origin.X
            let dy = y - origin.Y

            if dx * dx + dy * dy <= radius * radius && hasLineOfSight state.Map origin point then
                visible[y, x] <- true
                explored[y, x] <- true

    { state with
        VisibleTiles = visible
        ExploredTiles = explored }
