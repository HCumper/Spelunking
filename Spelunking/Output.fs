(* Maps gameplay and UI transitions onto future speech and sound output events. *)
module Spelunk.Output

type SoundCue =
    | UiOpen
    | UiClose
    | MoveStep
    | AttackHit
    | AttackKill
    | ErrorBump
    | WaitTurn
    | PlayerHurt
    | TargetLock
    | TargetReject

type OutputEvent =
    | PlaySound of SoundCue
    | SpeakText of string

let private classifyMessage (message: string) =
    if message.StartsWith("You kill") then
        [ PlaySound AttackKill ]
    elif message.StartsWith("You hit") then
        [ PlaySound AttackHit ]
    elif message.StartsWith("You shoot") || message.StartsWith("You blast") then
        [ PlaySound AttackHit ]
    elif message.StartsWith("You move") then
        [ PlaySound MoveStep ]
    elif message.StartsWith("You wait") then
        [ PlaySound WaitTurn ]
    elif message.StartsWith("The ") && message.Contains("hits you") then
        [ PlaySound PlayerHurt ]
    elif message.StartsWith("No target")
         || message.Contains("out of range")
         || message.StartsWith("The rock wall")
         || message.StartsWith("You bump")
         || message.StartsWith("Something is already there") then
        [ PlaySound TargetReject ]
    else
        []

let newMessages previousMessages nextMessages =
    let previousMessages = previousMessages |> Set.ofList

    nextMessages
    |> List.filter (fun message -> not (Set.contains message previousMessages))
    |> List.rev

let messageEvents previousMessages nextMessages =
    newMessages previousMessages nextMessages
    |> List.collect classifyMessage

let modalEvents openedModal closedModal =
    [ if openedModal then
          PlaySound UiOpen
      if closedModal then
          PlaySound UiClose ]

let combine previousMessages nextMessages openedModal closedModal =
    let messageEvents =
        messageEvents previousMessages nextMessages

    let modalEvents = modalEvents openedModal closedModal

    modalEvents @ messageEvents
