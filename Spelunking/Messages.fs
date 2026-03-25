(* Shared helpers for mutating the bounded in-game message log. *)
module Spelunk.Messages

open Spelunk.Model

let addMessage message state =
    { state with Messages = message :: state.Messages |> List.truncate 6 }
