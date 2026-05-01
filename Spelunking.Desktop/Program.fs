(* Program entry point that configures SadConsole and launches the root UI screen. *)
module Spelunk.Program

open System.IO
open SadConsole
open SadConsole.Configuration
open Spelunk.Config
open Spelunk.Ui

let private configureFont (builder: Builder) =
    let validatePath description path =
        if not (File.Exists path) then
            invalidOp $"Configured {description} font file does not exist: {path}"

    match tileFontPath () with
    | Some path ->
        validatePath "tile" path

        builder.ConfigureFonts(fun (fonts: FontConfig) _ ->
            fonts.UseBuiltinFontExtended()
            fonts.AddExtraFonts([| path |]))
    | None ->
        builder.ConfigureFonts(true)

let private desktopFontSize settingName (value: string) : IFont.Sizes =
    match value.Trim().ToLowerInvariant() with
    | "quarter" -> IFont.Sizes.Quarter
    | "half" -> IFont.Sizes.Half
    | "one" -> IFont.Sizes.One
    | "two" -> IFont.Sizes.Two
    | "three" -> IFont.Sizes.Three
    | "four" -> IFont.Sizes.Four
    | value -> invalidOp $"Unsupported Window:{settingName} value '{value}'."

let private tileFont (host: GameHost) : (IFont * IFont.Sizes) option =
    match tileFontPath () with
    | Some path ->
        let expectedName = Path.GetFileNameWithoutExtension path

        host.Fonts.Values
        |> Seq.tryFind (fun font -> font.Name = expectedName)
        |> Option.map (fun font -> font, desktopFontSize "TileFontSize" (configuredFontSizeName (windowSettings ()).TileFontSize))
    | None -> None

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
    let builder =
        builder.SetStartingScreen(fun host ->
            RootScreen(host.ScreenCellsX, host.ScreenCellsY, tileFont host, host.DefaultFont, host.DefaultFontSize) :> IScreenObject)
    let builder = builder.IsStartingScreenFocused(true)

    let configuredBuilder = configureFont builder

    configuredBuilder.SetDefaultFontSize(desktopFontSize "TextFontSize" (configuredFontSizeName (windowSettings ()).TextFontSize)).Run()

    0
