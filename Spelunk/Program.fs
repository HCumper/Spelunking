module Spelunk.Program

open SadConsole
open SadConsole.Configuration
open Spelunk.Ui

[<EntryPoint>]
let main _ =
    Settings.WindowTitle <- "Spelunk"

    Builder
        .GetBuilder()
        .SetWindowSizeInCells(40, 29)
        .SetStartingScreen<RootScreen>()
        .IsStartingScreenFocused(true)
        .ConfigureFonts(true)
        .Run()

    0
