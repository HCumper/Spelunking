(* Program entry point that configures SadConsole and launches the root UI screen. *)
module Spelunk.Program

open System.IO
open SadConsole
open SadConsole.Configuration
open Spelunk.Config
open Spelunk.Ui

let private configureFont (builder: Builder) =
    match defaultFontPath () with
    | Some path ->
        if not (File.Exists path) then
            invalidOp $"Configured font file does not exist: {path}"

        builder.ConfigureFonts(path, [||])
    | None ->
        builder.ConfigureFonts(true)

[<EntryPoint>]
let main _ =
    Settings.WindowTitle <- "Spelunk"
    let window = windowSettings ()

    let builder = Builder.GetBuilder()
    let builder =
        builder.ConfigureWindow(fun config _ _ ->
            let mutable screenWidth = 0
            let mutable screenHeight = 0
            config.GetDeviceScreenSize(&screenWidth, &screenHeight)
            // The game uses the monitor size and lets the UI derive panel sizes from the resulting cell grid.
            config.SetWindowSizeInPixels(screenWidth, screenHeight)
            config.Fullscreen <- window.Fullscreen
            config.BorderlessWindowedFullscreen <- window.BorderlessWindowedFullscreen)
    let builder = builder.SetStartingScreen(fun host -> RootScreen(host.ScreenCellsX, host.ScreenCellsY) :> IScreenObject)
    let builder = builder.IsStartingScreenFocused(true)

    let configuredBuilder = configureFont builder

    configuredBuilder.SetDefaultFontSize(defaultFontSize ()).Run()

    0
