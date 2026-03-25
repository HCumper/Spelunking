(* Program entry point that configures SadConsole and launches the root UI screen. *)
module Spelunk.Program

open SadConsole
open SadConsole.Configuration
open Spelunk.Config
open Spelunk.Ui

[<EntryPoint>]
let main _ =
    Settings.WindowTitle <- "Spelunk"
    let window = windowSettings ()

    Builder
        .GetBuilder()
        .ConfigureWindow(fun config _ _ ->
            let mutable screenWidth = 0
            let mutable screenHeight = 0
            config.GetDeviceScreenSize(&screenWidth, &screenHeight)
            // The game uses the monitor size and lets the UI derive panel sizes from the resulting cell grid.
            config.SetWindowSizeInPixels(screenWidth, screenHeight)
            config.Fullscreen <- window.Fullscreen
            config.BorderlessWindowedFullscreen <- window.BorderlessWindowedFullscreen)
        .SetStartingScreen(fun host -> RootScreen(host.ScreenCellsX, host.ScreenCellsY) :> IScreenObject)
        .IsStartingScreenFocused(true)
        .ConfigureFonts(true)
        .SetDefaultFontSize(defaultFontSize ())
        .Run()

    0
