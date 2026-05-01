(* Provides desktop implementations of the core effect boundaries. *)
module Spelunk.DesktopServices

open System
open System.Speech.Synthesis
open Spelunk.Config
open Spelunk.Model
open Spelunk.Services

let private createSpeechService () =
    let settings = speechSettings ()

    if not settings.Enabled then
        fun (_: string) -> ()
    else
        try
            let synthesizer = new SpeechSynthesizer()
            synthesizer.SetOutputToDefaultAudioDevice()
            synthesizer.Rate <- max -10 (min 10 settings.Rate)
            synthesizer.Volume <- max 0 (min 100 settings.Volume)

            fun text ->
                if not (String.IsNullOrWhiteSpace text) then
                    synthesizer.SpeakAsyncCancelAll()
                    synthesizer.SpeakAsync(text) |> ignore
        with ex ->
            Console.Error.WriteLine($"Speech disabled: {ex.Message}")
            fun (_: string) -> ()

let liveServices : Services =
    { SaveGame = fun (state, history) -> Spelunk.Save.saveGame state history
      TryLoadGame = Spelunk.Save.tryLoadGame
      Speak = createSpeechService () }
