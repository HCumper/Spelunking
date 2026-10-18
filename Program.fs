open System.Speech.Synthesis
[<EntryPoint>]
let main _ =
  let s = new SpeechSynthesizer()
  s.Volume <- 100
  s.Rate <- 0
  0
