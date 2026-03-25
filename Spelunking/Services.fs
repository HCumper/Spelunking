(* Defines external effect boundaries so application logic does not call I/O implementations directly. *)
module Spelunk.Services

open Spelunk.Model

type Services =
    { SaveGame: GameState * GameState list -> unit
      TryLoadGame: unit -> (GameState * GameState list) option }

let liveServices : Services =
    { SaveGame = fun (state, history) -> Spelunk.Save.saveGame state history
      TryLoadGame = Spelunk.Save.tryLoadGame }
