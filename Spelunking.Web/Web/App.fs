(* Elmish model and update loop for the static Bolero host. *)
module Spelunking.Web.App

open Elmish
open Spelunk.Application
open Spelunk.Model
open Spelunk.Output
open Spelunk.Services
open Spelunk.SessionActions

type Model =
    { Session: Session
      LastEvents: OutputEvent list }

type Message =
    | KeyDown of string
    | NoOp

let private services : Services =
    { SaveGame = fun (state, history) -> Spelunking.Web.Storage.saveGame (state, history)
      TryLoadGame = Spelunking.Web.Storage.tryLoadGame
      Speak = Spelunking.Web.Speech.speak }

let init _ =
    { Session = initialSession ()
      LastEvents = [] },
    Cmd.none

let private addMessage message state =
    { state with Messages = message :: state.Messages |> List.truncate 6 }

let private keepBrowserSession model events =
    let state =
        model.Session.State
        |> addMessage "The browser session is still open."

    { model with
        Session = { model.Session with State = state; Modal = NoModal }
        LastEvents = events },
    Cmd.none

let update message model =
    match message with
    | NoOp ->
        model, Cmd.none
    | KeyDown key ->
        let intent =
            Spelunking.Web.Input.intentFromKey key
            |> normalizeIntent model.Session

        let transition = applyIntent services intent model.Session

        match transition.NextSession with
        | Some session ->
            { model with
                Session = session
                LastEvents = transition.Events },
            Cmd.none
        | None ->
            keepBrowserSession model transition.Events
