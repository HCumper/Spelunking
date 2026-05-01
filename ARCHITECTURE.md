# Architecture

`Spelunk` is organized around a shared F# game core with host-specific presentation projects. The desktop host uses SadConsole/MonoGame, and the web host is a static Bolero WebAssembly client shell.

## High-Level Diagram

```mermaid
flowchart TD
    Desktop["Spelunking.Desktop\nSadConsole startup, rendering, speech"] --> Core["Spelunking.Core\nShared game rules"]
    Web["Spelunking.Web\nBolero WebAssembly shell"] --> Core
    Tests["Spelunking.Tests\nxUnit + FsUnit"] --> Core

    Core --> SessionActions["SessionActions.fs\nIntent handling, modal flow,\nrun, save/load dispatch"]
    SessionActions --> Domain["Domain.fs\nTop-level state transitions,\nworld changes"]
    Domain --> Simulation["Simulation.fs\nMovement, attacks,\nmonster turns"]
    Domain --> Visibility["Visibility.fs\nLOS, visible/explored tiles"]
    Domain --> World["World.fs / Dungeon.fs\nMap generation and layout"]
    Domain --> Combat["Combat.fs\nDamage and combat math"]
    Domain --> SessionHistory["SessionHistory.fs\nTime shifter rewind history"]
    SessionActions --> Application["Application.fs\nSession, intent,\ntransition types"]

    Domain --> Model["Model.fs\nCore domain types"]
    Simulation --> Model
    Visibility --> Model
    World --> Model
    Combat --> Model
    SessionHistory --> Model
    Application --> Model

    SessionActions --> Services["Services.fs\nEffect boundary"]
    Desktop --> DesktopServices["Spelunking.Desktop/Services.fs\nDesktop save/load and speech"]
    DesktopServices --> Services
    DesktopServices --> Save["Save.fs\nJSON serialization"]
    Config["Config.fs\nApp settings,\nMonsters.csv, Weapons.csv"] --> Domain
    Config --> Desktop
    Data["Appsettings.json\nMonsters.csv\nWeapons.csv"] --> Config
```

## Runtime Flow

```mermaid
sequenceDiagram
    participant P as Desktop Program.fs
    participant U as Ui.fs
    participant S as Core SessionActions.fs
    participant D as Core Domain.fs
    participant Sim as Core Simulation.fs
    participant V as Core Visibility.fs
    participant Sv as Desktop Services.fs

    P->>U: Create RootScreen
    U->>U: Poll keyboard/mouse
    U->>S: Apply player intent
    S->>D: Update session/game state
    D->>Sim: Resolve movement/combat/monster turns
    D->>V: Recompute visible/explored tiles
    D-->>S: Return next state + output events
    S-->>U: Updated session
    U->>Sv: Speak / save / load as needed
    U->>U: Render stats, map, messages, overlays
```

## Module Boundaries

- Shared core:
  - `Spelunking.Core/Model.fs`
  - `Spelunking.Core/World.fs`
  - `Spelunking.Core/Dungeon.fs`
  - `Spelunking.Core/Visibility.fs`
  - `Spelunking.Core/Combat.fs`
  - `Spelunking.Core/Simulation.fs`
  - `Spelunking.Core/Domain.fs`
  - `Spelunking.Core/Application.fs`
  - `Spelunking.Core/SessionActions.fs`
  - `Spelunking.Core/SessionHistory.fs`
  - `Spelunking.Core/Input.fs`
  - `Spelunking.Core/Overlay.fs`
  - `Spelunking.Core/Save.fs`
  - `Spelunking.Core/Config.fs`
- Desktop host:
  - `Spelunking.Desktop/Program.fs`
  - `Spelunking.Desktop/Ui.fs`
  - `Spelunking.Desktop/Appearance.fs`
  - `Spelunking.Desktop/Services.fs`
- Web host:
  - `Spelunking.Web/Main.fs`
  - `Spelunking.Web/Startup.fs`
  - `Spelunking.Web/wwwroot/`

## Design Notes

- The preferred dependency direction is inward toward `Spelunking.Core`.
- Host projects should render state and trigger effects, not own gameplay rules.
- Host-specific services should stay thin and effect-focused.
- Gameplay content belongs in CSV/JSON data files rather than hardcoded tables where possible.
