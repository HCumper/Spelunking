(* Procedural room-and-corridor generator that produces a floor mask and accepted room list. *)
module Spelunk.Dungeon

open System

type Room =
    { Left: int
      Top: int
      Width: int
      Height: int }

type Layout =
    { Width: int
      Height: int
      Floors: bool[,]
      Rooms: Room list }

type GeneratorConfig =
    { MapWidth: int
      MapHeight: int
      RoomAttempts: int
      MinRoomWidth: int
      MaxRoomWidth: int
      MinRoomHeight: int
      MaxRoomHeight: int
      MaxOverlappingRooms: int }

let private clampSize minValue maxValue available =
    let boundedMax = min maxValue available
    let boundedMin = min minValue boundedMax
    boundedMin, boundedMax

let private intersects first second =
    first.Left < second.Left + second.Width
    && first.Left + first.Width > second.Left
    && first.Top < second.Top + second.Height
    && first.Top + first.Height > second.Top

let private mergeRooms first second =
    let left = min first.Left second.Left
    let top = min first.Top second.Top
    let right = max (first.Left + first.Width) (second.Left + second.Width)
    let bottom = max (first.Top + first.Height) (second.Top + second.Height)

    { Left = left
      Top = top
      Width = right - left
      Height = bottom - top }

let private roomCenter room =
    room.Left + room.Width / 2, room.Top + room.Height / 2

let private carveRoom (floors: bool[,]) room =
    for y in room.Top .. room.Top + room.Height - 1 do
        for x in room.Left .. room.Left + room.Width - 1 do
            floors[y, x] <- true

let private carveHorizontalTunnel (floors: bool[,]) y startX endX =
    for x in min startX endX .. max startX endX do
        floors[y, x] <- true

let private carveVerticalTunnel (floors: bool[,]) x startY endY =
    for y in min startY endY .. max startY endY do
        floors[y, x] <- true

let private carveConnection (floors: bool[,]) previousRoom nextRoom =
    let startX, startY = roomCenter previousRoom
    let endX, endY = roomCenter nextRoom

    carveHorizontalTunnel floors startY startX endX
    carveVerticalTunnel floors endX startY endY

let private randomRoom (random: Random) config =
    let availableWidth = max 1 (config.MapWidth - 2)
    let availableHeight = max 1 (config.MapHeight - 2)
    let minRoomWidth, maxRoomWidth = clampSize config.MinRoomWidth config.MaxRoomWidth availableWidth
    let minRoomHeight, maxRoomHeight = clampSize config.MinRoomHeight config.MaxRoomHeight availableHeight
    let width = random.Next(minRoomWidth, maxRoomWidth + 1)
    let height = random.Next(minRoomHeight, maxRoomHeight + 1)
    let left = random.Next(1, config.MapWidth - width)
    let top = random.Next(1, config.MapHeight - height)

    { Left = left
      Top = top
      Width = width
      Height = height }

let generate config =
    let random = Random.Shared
    let floors = Array2D.create config.MapHeight config.MapWidth false

    let rooms, _ =
        (([], 0), [ 1 .. max 0 config.RoomAttempts ])
        ||> List.fold (fun (rooms, overlapsUsed) _ ->
            let candidate = randomRoom random config
            let overlapping, remaining = rooms |> List.partition (intersects candidate)

            match overlapping with
            | [] ->
                candidate :: rooms, overlapsUsed
            | _ when overlapsUsed < max 0 config.MaxOverlappingRooms ->
                // Early overlaps expand the existing footprint instead of discarding the candidate.
                let extended = (candidate, overlapping) ||> List.fold mergeRooms
                extended :: remaining, overlapsUsed + 1
            | _ ->
                rooms, overlapsUsed)

    let orderedRooms = rooms |> List.rev

    orderedRooms |> List.iter (carveRoom floors)

    orderedRooms
    |> List.pairwise
    |> List.iter (fun (previousRoom, nextRoom) -> carveConnection floors previousRoom nextRoom)

    { Width = config.MapWidth
      Height = config.MapHeight
      Floors = floors
      Rooms = orderedRooms }
