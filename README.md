# Spelunk

Doctor Who-themed dungeon crawler built in F# with SadConsole.

## Current Game

- Procedurally generated scrolling dungeon
- Fog of war with explored-memory tiles
- Melee and ranged combat
- Weighted monster spawning by world
- Time shifter that rewinds up to 10 turns
- JSON save/load

## Controls

- `WASD`, arrow keys, or numpad `1-9`: move
- `Space` or numpad `5`: wait
- `L`: look
- `F`: target/fire
- `I`: inventory
- `T`: time shifter
- `F5`: save
- `F9`: load
- `Q`: quit

## Rules Summary

- Movement is grid-based. The player can move in 8 directions.
- Moving into a monster performs a melee attack instead of movement.
- Monsters currently move in 4 directions only.
- Look mode is directional. Choose a direction and it reports the first visible non-floor thing on that line. If nothing is visible, it reports that.
- Target mode lets you aim the equipped weapon. Confirming fires if the target point is within weapon range.
- Vision is limited by line of sight and the configured sight radius. Walls block sight. Previously seen tiles remain explored.
- Player passive regeneration restores `1 HP` every `10` turns.
- Killing a monster heals the player for half of that monster's `MaxHp`, rounded up.

## Combat

- Melee damage is based on the attacker's `Strength`.
- `Strength 1-10` deals `1` damage, `11-20` deals `2`, `21-30` deals `3`, and so on.
- Weapon attacks use the equipped weapon's `Damage`.
- There is currently no armor, dodge, resistance, or damage randomness.
- A monster's toughness is mostly its `MaxHp`.

## Speed

- Speed is an action-rate stat, not tiles-per-turn movement.
- Each monster gains energy equal to its `Speed` each player turn.
- `10` energy allows one monster action.
- One action is either one move or one melee attack.
- `Speed 10` is the baseline rate: about one action per player turn.
- `Speed above 10` can produce multiple actions in one player turn.
- `Speed below 10` means the monster acts less often.
- `Speed 0` means the monster never acts unless given stored energy.

## Compared With Moria

- This game is currently simpler and more deterministic than classic Moria.
- Melee damage here is driven directly by `Strength`; Moria uses richer attack and equipment rules with more randomness.
- Ranged attacks here use fixed weapon damage; Moria-style combat usually combines hit chance, launcher/ammo interactions, and randomized damage.
- Monsters here mostly pursue the player's exact position greedily; Moria has broader creature behavior even though it is still a classic turn-based roguelike.
- Monster toughness here is mostly just `MaxHp`; Moria creatures are differentiated more by combat tables, abilities, and defenses.
- Speed here is implemented as a simple energy system where `10` energy buys one move or melee attack; Moria's speed model is turn-order based but less directly exposed in those terms.
- This game currently gives passive healing and kill-based healing; that is more forgiving and gamey than Moria's more traditional recovery model.

## Data Files

- App/runtime settings: [`Spelunking/Appsettings.json`](Spelunking/Appsettings.json)
- Monsters: [`Spelunking/Data/Monsters.csv`](Spelunking/Data/Monsters.csv)
- Weapons: [`Spelunking/Data/Weapons.csv`](Spelunking/Data/Weapons.csv)

`Monsters.csv` uses `GlyphCode` for each monster's SadConsole font glyph index. ASCII-compatible values such as `68` still render as familiar letters like `D`, while custom font sheets can use non-ASCII glyph cells.

Custom SadConsole `.font` files can be placed under `Spelunking/Fonts/` and selected with `Window:TileFontPath` in `Appsettings.json`, for example `"Fonts/MyTiles.font"`. Monster `GlyphCode` values then refer to cells in that selected tile font, while UI text continues to use SadConsole's built-in text font.
