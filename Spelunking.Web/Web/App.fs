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
      Camera: Position
      LastEvents: OutputEvent list }

type Message =
    | KeyDown of string
    | NoOp

let private services : Services =
    { SaveGame = fun (state, history) -> Spelunking.Web.Storage.saveGame (state, history)
      TryLoadGame = Spelunking.Web.Storage.tryLoadGame
      Speak = Spelunking.Web.Speech.speak }

let private viewportWidth = 72
let private viewportHeight = 30
let private cameraMargin = 10

let private focusPosition session =
    match session.Modal with
    | LookMode cursor
    | TargetMode cursor -> cursor
    | _ -> session.State.Player.Position

let private clampCameraAxis current focus viewportSize worldSize =
    let effectiveMargin =
        cameraMargin
        |> min (max 0 ((viewportSize - 1) / 2))

    let minFocus = current + effectiveMargin
    let maxFocus = current + viewportSize - effectiveMargin - 1
    let maxCamera = max 0 (worldSize - viewportSize)

    let next =
        if focus < minFocus then
            focus - effectiveMargin
        elif focus > maxFocus then
            focus - viewportSize + effectiveMargin + 1
        else
            current

    max 0 (min maxCamera next)

let private adjustCamera camera session =
    let focus = focusPosition session

    { X = clampCameraAxis camera.X focus.X viewportWidth session.State.Map.Width
      Y = clampCameraAxis camera.Y focus.Y viewportHeight session.State.Map.Height }

let init _ =
    let session = initialSession ()

    { Session = session
      Camera = adjustCamera { X = 0; Y = 0 } session
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
        Camera = adjustCamera model.Camera { model.Session with State = state; Modal = NoModal }
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
                Camera = adjustCamera model.Camera session
                LastEvents = transition.Events },
            Cmd.none
        | None ->
            keepBrowserSession model transition.Events
