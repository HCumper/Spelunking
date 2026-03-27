# AGENTS.md

## Purpose

This repository contains `Spelunk`, a Doctor Who-themed roguelike written in F# on top of SadConsole developed in Rider.

This file is for coding agents working in this repo. Prefer small, coherent changes that preserve the existing separation between pure game logic and side-effecting UI/services.

## Solution Layout

- `Spelunking/`
  - Main game project.
- `Spelunking.Tests/`
  - xUnit + FsUnit test project.
- `README.md`
  - User-facing project overview and gameplay notes.

## Main Code Structure

- `Model.fs`
  - Core domain types.
- `World.fs` / `Dungeon.fs`
  - World generation and room/corridor layout.
- `Visibility.fs`
  - LOS and explored/visible tile logic.
- `Combat.fs`
  - Damage and combat math.
- `Simulation.fs`
  - Movement, attacks, ranged fire, and monster turns.
- `Domain.fs`
  - World creation and top-level state transitions.
- `Application.fs`
  - Session, modal, intent, and transition types.
- `SessionActions.fs`
  - Intent handling, modal flow, run behavior, save/load dispatch.
- `SessionHistory.fs`
  - Time shifter history and rewind logic.
- `Ui.fs`
  - SadConsole panels, input polling, camera, rendering, and speech triggering.
- `Services.fs`
  - External effects such as save/load and speech.
- `Save.fs`
  - Save-game serialization.
- `Config.fs`
  - Loads app settings and CSV game data.

## Data Files

- `Spelunking/Appsettings.json`
  - Runtime and tuning settings.
- `Spelunking/Data/Monsters.csv`
  - Monster definitions.
- `Spelunking/Data/Weapons.csv`
  - Weapon definitions.

Keep gameplay content in `Data/`. Keep app/runtime settings in `Appsettings.json`.

## Working Conventions

- Preserve the pure/impure boundary:
  - Keep rules and state transitions in domain/application modules.
  - Keep file I/O, speech, and SadConsole interactions in services/UI.
- Do not reintroduce routine speech from the message log.
  - Speech is explicit and event-driven.
- Prefer extending existing modules over adding parallel duplicate helpers.
- Keep message-log mutations routed through `Messages.addMessage`.
- Keep tests updated with behavior changes; this repo already has useful regression coverage.

## Run / Build / Test

From the repo root:

```powershell
dotnet build Spelunking.sln /p:UseAppHost=false
dotnet test Spelunking.Tests\Spelunking.Tests.fsproj /p:UseAppHost=false
dotnet run --project Spelunking\Spelunking.fsproj /p:UseAppHost=false
```

## Change Guidance

- If you change player-visible terminology, update:
  - code strings
  - tests
  - README
  - data headers if the schema changed
- If you add a gameplay mechanic, look for corresponding updates in:
  - `Model.fs`
  - `Simulation.fs`
  - `Domain.fs`
  - `Save.fs`
  - tests
- If you add new monster or weapon fields, update:
  - CSV headers
  - `Config.fs` parsers
  - any affected save/test logic

## Testing Expectations

At minimum, run:

```powershell
dotnet test Spelunking.Tests\Spelunking.Tests.fsproj /p:UseAppHost=false
```

If you touch only docs, note that tests were not rerun.

## Current Gameplay Assumptions

- The game uses `World` terminology, not `Depth` or `Level`, in player-facing language.
- Entering the TARDIS generates the next world.
- The time shifter rewinds from in-memory history, not from disk saves.
- Running is `.` plus a numpad direction and stops on branches, blockers, world transition, or newly visible monsters.

