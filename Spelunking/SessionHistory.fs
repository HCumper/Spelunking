(* Maintains the bounded turn history used by the time shifter. *)
module Spelunk.SessionHistory

open Spelunk.Application
open Spelunk.Messages
open Spelunk.Model

let private recordsHistory command =
    match command with
    | Move _
    | Wait
    | FireAt _ -> true

let pushHistory state history =
    state :: history |> List.truncate 10


let recordAction command session =
    let history =
        if recordsHistory command then
            pushHistory session.State session.History
        else
            session.History

    { session with History = history }

let rewindSession turns session =
    let availableTurns = session.History.Length
    let actualTurns = max 1 (min turns availableTurns)

    match session.History |> List.tryItem (actualTurns - 1) with
    | Some priorState ->
        { session with
            State =
                { priorState with
                    Messages =
                        sprintf "The time shifter drags you back %d turn%s." actualTurns (if actualTurns = 1 then "" else "s")
                        :: priorState.Messages
                        |> List.truncate 6 }
            Modal = NoModal
            History = session.History |> List.skip actualTurns }
    | None ->
        { session with
            State = addMessage "The time shifter finds no earlier state." session.State
            Modal = NoModal }
