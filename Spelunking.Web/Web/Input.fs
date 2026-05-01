(* Browser keyboard mapping for the Bolero host. *)
module Spelunking.Web.Input

open Spelunk.Application
open Spelunk.Model

let intentFromKey (key: string) =
    match key.ToLowerInvariant() with
    | "f5" -> SaveGame
    | "f9" -> LoadGame
    | "q" -> Quit
    | "escape" -> Cancel
    | "enter" -> Confirm
    | "l" -> OpenLook
    | "f" -> OpenTarget
    | "i" -> OpenInventory
    | "t" -> UseTimeShifter
    | " " -> Act Wait
    | "arrowup"
    | "up"
    | "w" -> MoveCursor(0, -1)
    | "arrowdown"
    | "down"
    | "s" -> MoveCursor(0, 1)
    | "arrowleft"
    | "left"
    | "a" -> MoveCursor(-1, 0)
    | "arrowright"
    | "right"
    | "d" -> MoveCursor(1, 0)
    | "home" -> MoveCursor(-1, -1)
    | "pageup" -> MoveCursor(1, -1)
    | "end" -> MoveCursor(-1, 1)
    | "pagedown" -> MoveCursor(1, 1)
    | digit when digit.Length = 1 && digit[0] >= '0' && digit[0] <= '9' ->
        EnterDigit(int digit[0] - int '0')
    | "backspace" -> EraseDigit
    | _ -> Ignore
