(* Encapsulates combat-specific rules such as damage, kill rewards, and weapon ammo use. *)
module Spelunk.Combat

open Spelunk.Model

let private actionThreshold = 100

let clampHp actor hp =
    { actor with Hp = max 0 (min actor.MaxHp hp) }

let damageFor attacker =
    // Strength 100 corresponds to baseline 1 damage; larger values scale upward in whole points.
    max 1 ((attacker.Strength + actionThreshold - 1) / actionThreshold)

let attack attacker target =
    clampHp target (target.Hp - damageFor attacker)

let attackWithWeapon weapon target =
    clampHp target (target.Hp - max 1 weapon.Damage)

let addAmmo weapon delta =
    { weapon with
        Ammo =
            match weapon.Ammo with
            | Some ammo -> Some(max 0 (ammo + delta))
            | None -> None }

let killBoost maxHp =
    (maxHp + 1) / 2
