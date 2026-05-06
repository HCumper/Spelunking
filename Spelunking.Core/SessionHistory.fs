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

let private rewindableHistory session =
    let currentIncarnation = session.State.Player.Incarnation

    session.History
    |> List.takeWhile (fun priorState -> priorState.Player.Incarnation = currentIncarnation)

let availableRewindTurns session =
    rewindableHistory session |> List.length

let rewindSession turns session =
    let safeHistory = rewindableHistory session
    let availableTurns = safeHistory.Length
    let actualTurns = min turns availableTurns
    let hasRegenerationBoundary = safeHistory.Length < session.History.Length
    let limitedByRegeneration = hasRegenerationBoundary && actualTurns < turns

    let requestedState =
        if actualTurns < 1 then
            None
        else
            safeHistory |> List.tryItem (actualTurns - 1)

    match requestedState with
    | Some priorState ->
        let rewindMessage =
            sprintf "The time shifter drags you back %d turn%s." actualTurns (if actualTurns = 1 then "" else "s")

        let messages =
            if limitedByRegeneration then
                [ rewindMessage
                  "The shift is limited to the lifetime of this regeneration." ]
            else
                [ rewindMessage ]

        { session with
            State =
                { priorState with
                    Messages =
                        messages
                        @ priorState.Messages
                        |> List.truncate 6 }
            Modal = NoModal
            History = session.History |> List.skip actualTurns }
    | None ->
        { session with
            State =
                if turns < 1 then
                    addMessage "Enter a turn count from 1 to 10." session.State
                elif List.isEmpty session.History then
                    addMessage "The time shifter finds no earlier state." session.State
                else
                    { session.State with
                        Messages =
                            [ "The time shifter cannot reach before your regeneration."
                              "The shift is limited to the lifetime of this regeneration." ]
                            @ session.State.Messages
                            |> List.truncate 6 }
            Modal = NoModal }
