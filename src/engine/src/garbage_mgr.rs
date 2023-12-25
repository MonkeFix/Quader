use std::collections::VecDeque;
use rand::{Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;
use crate::cell_holder::CellHolder;
use crate::game_settings::AttackSettings;
use crate::replays::HardDropInfo;

#[derive(Debug)]
pub struct GarbageMgr {
    pub queue: VecDeque<i32>,
    // used for generating garbage holes,
    rng: ChaCha8Rng,
    last_garbage_x: Option<u32>,
    garbage_delay_ms: u32,
    attack_settings: AttackSettings
}

impl GarbageMgr {
    pub fn new(attack_settings: &AttackSettings) -> Self {
        Self {
            queue: VecDeque::default(),
            rng: SeedableRng::from_entropy(),
            last_garbage_x: None,
            garbage_delay_ms: attack_settings.garbage_delay_ms,
            attack_settings: *attack_settings
        }
    }

    pub fn attack(&mut self, damage: i32) {
        if damage > 0 {
            self.queue.push_back(damage);
        }
    }

    /// Sends garbage onto current board with specified `amount` of garbage rows and `messiness`.
    /// The higher the messiness, the more random the holes are.
    /// Messiness = 0 means that the hole will be at the same x coordinate within
    /// pending garbage rows.
    pub fn push_garbage(
        &mut self,
        amount: u32,
        _messiness: u32,
        cell_holder: &mut CellHolder
    ) {
        let width = cell_holder.width as u32;

        let garbage_hole_x: u32 = if let Some(gx) = self.last_garbage_x {
            // TODO: Use messiness
            gx
        } else {
            self.rng.gen_range(0..width)
        };

        for _ in 0..amount {
            cell_holder.push_garbage(garbage_hole_x);
        }
    }


    pub fn hard_drop(&mut self, cell_holder: &mut CellHolder, hard_drop_info: &HardDropInfo, outgoing_damage: i32) -> i32 {
        let mut result = 0;

        if !self.queue.is_empty() {
            if hard_drop_info.lines_cleared > 0 {

                //let damage_cancel = calculate_damage();
                let mut damage_cancel: i32 = outgoing_damage;
                let mut res = false;

                while !self.queue.is_empty() {
                    let damage = self.queue.pop_front().unwrap();
                    damage_cancel -= damage;

                    if damage_cancel <= 0 {
                        if damage_cancel != 0 {
                            self.queue.push_front(-damage_cancel);
                        }
                        res = true;
                        break;
                    }
                }

                if !res {
                    return damage_cancel.max(0);
                }

            } else if self.garbage_delay_ms == 0 {
                let dmg = self.queue.pop_front().unwrap() as u32;
                self.push_garbage(dmg, 0, cell_holder);
                self.garbage_delay_ms = self.attack_settings.garbage_delay_ms;
            }
        } else {
            // return full attack
            result = outgoing_damage;
        }

        result
    }

    pub fn update(&mut self, elapsed_ms: u32) {
        self.garbage_delay_ms = self.garbage_delay_ms.saturating_sub(elapsed_ms);
    }
}