/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full licence text.
 */

use std::collections::VecDeque;
use rand::{Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;
use serde::{Deserialize, Serialize};
use crate::cell_holder::CellHolder;
use crate::game_settings::AttackSettings;
use crate::time_mgr::{TimeMgr};

#[derive(Default, Debug, Copy, Clone, Serialize, Deserialize)]
pub struct IncomingDamage {
    pub amount: i32,
    pub delay: u32,
    pub hole_x: u32
}

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct GarbageHardDropResult {
    pub in_damage_queue: Vec<IncomingDamage>,
    pub out_damage: i32
}

impl IncomingDamage {
    pub fn new(amount: i32, delay_ms: u32) -> Self {
        Self {
            amount,
            delay: delay_ms,
            hole_x: 0
        }
    }
}

#[derive(Debug)]
pub struct GarbageMgr {
    pub queue: VecDeque<IncomingDamage>,
    // used for generating garbage holes,
    rng: ChaCha8Rng,
    last_garbage_x: Option<u32>,
    attack_settings: AttackSettings
}

impl GarbageMgr {
    pub fn new(attack_settings: &AttackSettings) -> Self {
        Self {
            queue: VecDeque::default(),
            rng: SeedableRng::from_entropy(),
            last_garbage_x: None,
            attack_settings: *attack_settings
        }
    }

    pub fn attack(&mut self, width: usize, damage: i32) {
        if damage > 0 {
            let hole_x = self.rng.gen_range(0..width) as u32;
            self.queue.push_back(IncomingDamage {
                amount: damage,
                delay: self.attack_settings.garbage_delay_ms,
                hole_x
            });
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

    pub fn push_garbage_at(&mut self, amount: u32, hole_x: u32, cell_holder: &mut CellHolder) {
        for _ in 0..amount {
            cell_holder.push_garbage(hole_x);
        }
    }


    pub fn hard_drop(&mut self, lines_cleared: u32, outgoing_damage: i32) -> GarbageHardDropResult {

        let mut result = GarbageHardDropResult {
            in_damage_queue: vec![],
            out_damage: outgoing_damage
        };

        // if the queue is empty then deal damage to the enemies
        if self.queue.is_empty() {
            return result;
        }

        // accumulate total incoming damage
        let mut incoming_dmg = VecDeque::new();
        loop {
            let recv_dmg = self.queue.front();
            if let Some(dmg) = recv_dmg {
                if dmg.delay > 0 {
                    break;
                }

                let recv_dmg = self.queue.pop_front().unwrap();
                incoming_dmg.push_back(recv_dmg);
            } else {
                break;
            }
        }

        // if player hasn't cleared any lines, than push garbage onto his board
        if lines_cleared == 0 {
            return GarbageHardDropResult {
                in_damage_queue: incoming_dmg.into_iter().collect(),
                out_damage: 0
            };
        }

        while !incoming_dmg.is_empty() {
            let mut recv_dmg = incoming_dmg.pop_front().unwrap();
            if recv_dmg.amount <= result.out_damage {
                result.out_damage -= recv_dmg.amount;
            } else {
                recv_dmg.amount -= result.out_damage;
                incoming_dmg.push_front(recv_dmg);

                result.out_damage = 0;

                for dmg in incoming_dmg {
                    self.queue.push_front(dmg);
                }

                return result;
            }
        }

        // see if queue still contains damage entries with delay > 0
        while !self.queue.is_empty() {
            let dmg = self.queue.pop_front().unwrap();
            result.out_damage -= dmg.amount;
            if result.out_damage == 0 {
                break;
            }
            if result.out_damage < 0 {
                self.queue.push_front(IncomingDamage {
                    amount: -result.out_damage,
                    delay: dmg.delay,
                    hole_x: dmg.hole_x
                });
                result.out_damage = 0;
                break;
            }
        }

        result
    }

    pub fn update(&mut self, time_mgr: &TimeMgr) {
        for dmg in self.queue.iter_mut() {
            dmg.delay = dmg.delay.saturating_sub(time_mgr.elapsed_sec as u32);
        }
    }

    pub fn reset(&mut self) {
        self.queue.clear();
        self.last_garbage_x = None;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const ATTACK_SETTINGS: AttackSettings = AttackSettings {
        garbage_delay_ms: 500,
        lines_0: 0, lines_1: 0, lines_2: 1, lines_3: 2, lines_4: 5,
        t_spin_single_mini: 0, t_spin_single: 2, t_spin_double: 4, t_spin_triple: 6,
        all_clear: 8, combos: [1,2,3,4,5], b2bs: [1,2,3,4,5]
    };

    fn create_garbage_mgr() -> GarbageMgr {
        GarbageMgr::new(&ATTACK_SETTINGS)
    }

    fn id(amount: i32, delay: u32) -> IncomingDamage {
        IncomingDamage {
            amount, delay, hole_x: 0
        }
    }

    #[test]
    fn hard_drop_no_incoming_no_outgoing() {
        let mut g = create_garbage_mgr();

        let dmg = g.hard_drop(0, 0);
        assert_eq!(dmg.out_damage, 0);
        assert_eq!(dmg.in_damage_queue.len(), 0);
    }

    #[test]
    fn hard_drop_no_incoming_some_outgoing() {
        let mut g = create_garbage_mgr();

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg.out_damage, 4);
        assert_eq!(dmg.in_damage_queue.len(), 0);
    }

    #[test]
    fn hard_drop_outgoing_higher_than_single_incoming_no_delay() {
        let mut g = create_garbage_mgr();
        g.queue.push_back(id(2, 0));

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg.out_damage, 2);
        assert_eq!(dmg.in_damage_queue.len(), 0);
        assert_eq!(g.queue.len(), 0);
    }

    #[test]
    fn hard_drop_outgoing_higher_than_two_incoming_no_delay() {
        let mut g = create_garbage_mgr();
        g.queue.push_back(id(1, 0));
        g.queue.push_back(id(2, 0));

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg.out_damage, 1);
        assert_eq!(g.queue.len(), 0);
    }

    #[test]
    fn hard_drop_outgoing_higher_than_single_incoming_with_delay() {
        let mut g = create_garbage_mgr();
        g.queue.push_back(id(2, 100));

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg.out_damage, 2);
        assert_eq!(g.queue.len(), 0);
    }

    #[test]
    fn hard_drop_outgoing_higher_than_two_incoming_with_delay() {
        let mut g = create_garbage_mgr();
        g.queue.push_back(id(2, 100));
        g.queue.push_back(id(1, 150));

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg.out_damage, 1);
        assert_eq!(g.queue.len(), 0);
    }

    #[test]
    fn hard_drop_outgoing_equals_two_incoming() {
        let mut g = create_garbage_mgr();
        g.queue.push_back(id(2, 0));
        g.queue.push_back(id(2, 150));

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg.out_damage, 0);
        assert_eq!(g.queue.len(), 0);
    }

    #[test]
    fn hard_drop_outgoing_lower_than_incoming_no_delay() {
        let mut g = create_garbage_mgr();
        g.queue.push_back(id(10, 0));

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg.out_damage, 0);
        assert_eq!(g.queue.len(), 1);
        assert_eq!(g.queue[0].amount, 6);

        let dmg = g.hard_drop(1, 1);
        assert_eq!(dmg.out_damage, 0);
        assert_eq!(g.queue.len(), 1);
        assert_eq!(g.queue[0].amount, 5);

        let dmg = g.hard_drop(0, 0);
        assert_eq!(dmg.out_damage, -5);
        assert_eq!(g.queue.len(), 0);
    }

    #[test]
    fn hard_drop_outgoing_lower_than_incoming_with_delay() {
        let mut g = create_garbage_mgr();
        g.queue.push_back(id(10, 100));

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg.out_damage, 0);
        assert_eq!(g.queue.len(), 1);
        assert_eq!(g.queue[0].amount, 6);

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg.out_damage, 0);
        assert_eq!(g.queue.len(), 1);
        assert_eq!(g.queue[0].amount, 2);

        let dmg = g.hard_drop(0, 0);
        assert_eq!(dmg.out_damage, 0);
        assert_eq!(g.queue.len(), 1);
        assert_eq!(g.queue[0].amount, 2);
        assert_eq!(g.queue[0].delay, 100);

        g.queue[0].delay = 0;
        let dmg = g.hard_drop(0, 0);
        assert_eq!(dmg.out_damage, -2);
        assert_eq!(g.queue.len(), 0);
    }

    #[test]
    fn hard_drop_outgoing_lower_than_multiple_incoming() {
        let mut g = create_garbage_mgr();
        g.queue.push_back(id(1, 0));
        g.queue.push_back(id(2, 0));
        g.queue.push_back(id(3, 0));
        g.queue.push_back(id(1, 50));
        g.queue.push_back(id(2, 100));
        g.queue.push_back(id(3, 150));

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg.out_damage, 0);
        assert_eq!(g.queue.len(), 4);
        assert_eq!(g.queue[0].amount, 2);

        let dmg = g.hard_drop(0, 0);
        assert_eq!(dmg.out_damage, -2);
        assert_eq!(g.queue.len(), 3);
        assert_eq!(g.queue[0].amount, 1);

        let mut tm = TimeMgr::new();
        tm.update(100.0);
        g.update(&tm);

        let dmg = g.hard_drop(0, 0);
        assert_eq!(dmg.out_damage, -3);
        assert_eq!(g.queue.len(), 1);
        assert_eq!(g.queue[0].amount, 3);
        assert_eq!(g.queue[0].delay, 50);

        let dmg = g.hard_drop(4, 10);
        assert_eq!(dmg.out_damage, 7);
        assert_eq!(g.queue.len(), 0);
    }
}