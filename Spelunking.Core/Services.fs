(* Defines effect boundaries used by session actions without committing the core to any host implementation. *)
module Spelunk.Services

open Spelunk.Model

type Services =
    { SaveGame: GameState * GameState list -> unit
      TryLoadGame: unit -> (GameState * GameState list) option
      Speak: string -> unit }
