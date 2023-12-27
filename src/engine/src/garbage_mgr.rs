use std::collections::VecDeque;
use rand::{Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;
use crate::cell_holder::CellHolder;
use crate::game_settings::AttackSettings;

#[derive(Debug, Copy, Clone)]
pub struct IncomingDamage {
    pub amount: i32,
    pub delay: u32
}

impl IncomingDamage {
    pub fn new(amount: i32, delay_ms: u32) -> Self {
        Self {
            amount,
            delay: delay_ms
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

    pub fn attack(&mut self, damage: i32) {
        if damage > 0 {
            self.queue.push_back(IncomingDamage {
                amount: damage,
                delay: self.attack_settings.garbage_delay_ms
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


    pub fn hard_drop(&mut self, lines_cleared: u32, outgoing_damage: i32) -> i32 {

        // if the queue is empty then deal damage to the enemies
        if self.queue.is_empty() {
            return outgoing_damage;
        }

        // accumulate total incoming damage
        let mut incoming_dmg = 0;
        loop {
            let recv_dmg = self.queue.front();
            if let Some(dmg) = recv_dmg {
                if dmg.delay > 0 {
                    break;
                }

                let recv_dmg = self.queue.pop_front().unwrap();
                incoming_dmg += recv_dmg.amount;
            } else {
                break;
            }
        }

        // if player hasn't cleared any lines, than push garbage onto his board
        if lines_cleared == 0 {
            return -incoming_dmg;
        }

        let mut dt = outgoing_damage - incoming_dmg;
        if dt == 0 {
            return 0;
        }

        if dt < 0 {
            self.queue.push_front(IncomingDamage { amount: -dt, delay: 0 });
            return 0;
        }

        // see if queue still contains damage entries with delay > 0
        while !self.queue.is_empty() {
            let dmg = self.queue.pop_front().unwrap();
            dt -= dmg.amount;
            if dt == 0 {
                break;
            }
            if dt < 0 {
                self.queue.push_front(IncomingDamage { amount: -dt, delay: dmg.delay });
                dt = 0;
                break;
            }
        }

        dt
    }

    pub fn update(&mut self, elapsed_ms: u32) {
        for dmg in self.queue.iter_mut() {
            dmg.delay = dmg.delay.saturating_sub(elapsed_ms);
        }
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
            amount, delay
        }
    }

    #[test]
    fn hard_drop_no_incoming_no_outgoing() {
        let mut g = create_garbage_mgr();

        let dmg = g.hard_drop(0, 0);
        assert_eq!(dmg, 0);
    }

    #[test]
    fn hard_drop_no_incoming_some_outgoing() {
        let mut g = create_garbage_mgr();

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg, 4);
    }

    #[test]
    fn hard_drop_outgoing_higher_than_single_incoming_no_delay() {
        let mut g = create_garbage_mgr();
        g.queue.push_back(id(2, 0));

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg, 2);
        assert_eq!(g.queue.len(), 0);
    }

    #[test]
    fn hard_drop_outgoing_higher_than_two_incoming_no_delay() {
        let mut g = create_garbage_mgr();
        g.queue.push_back(id(1, 0));
        g.queue.push_back(id(2, 0));

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg, 1);
        assert_eq!(g.queue.len(), 0);
    }

    #[test]
    fn hard_drop_outgoing_higher_than_single_incoming_with_delay() {
        let mut g = create_garbage_mgr();
        g.queue.push_back(id(2, 100));

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg, 2);
        assert_eq!(g.queue.len(), 0);
    }

    #[test]
    fn hard_drop_outgoing_higher_than_two_incoming_with_delay() {
        let mut g = create_garbage_mgr();
        g.queue.push_back(id(2, 100));
        g.queue.push_back(id(1, 150));

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg, 1);
        assert_eq!(g.queue.len(), 0);
    }

    #[test]
    fn hard_drop_outgoing_equals_two_incoming() {
        let mut g = create_garbage_mgr();
        g.queue.push_back(id(2, 0));
        g.queue.push_back(id(2, 150));

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg, 0);
        assert_eq!(g.queue.len(), 0);
    }

    #[test]
    fn hard_drop_outgoing_lower_than_incoming_no_delay() {
        let mut g = create_garbage_mgr();
        g.queue.push_back(id(10, 0));

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg, 0);
        assert_eq!(g.queue.len(), 1);
        assert_eq!(g.queue[0].amount, 6);

        let dmg = g.hard_drop(1, 1);
        assert_eq!(dmg, 0);
        assert_eq!(g.queue.len(), 1);
        assert_eq!(g.queue[0].amount, 5);

        let dmg = g.hard_drop(0, 0);
        assert_eq!(dmg, -5);
        assert_eq!(g.queue.len(), 0);
    }

    #[test]
    fn hard_drop_outgoing_lower_than_incoming_with_delay() {
        let mut g = create_garbage_mgr();
        g.queue.push_back(id(10, 100));

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg, 0);
        assert_eq!(g.queue.len(), 1);
        assert_eq!(g.queue[0].amount, 6);

        let dmg = g.hard_drop(2, 4);
        assert_eq!(dmg, 0);
        assert_eq!(g.queue.len(), 1);
        assert_eq!(g.queue[0].amount, 2);

        let dmg = g.hard_drop(0, 0);
        assert_eq!(dmg, 0);
        assert_eq!(g.queue.len(), 1);
        assert_eq!(g.queue[0].amount, 2);
        assert_eq!(g.queue[0].delay, 100);

        g.queue[0].delay = 0;
        let dmg = g.hard_drop(0, 0);
        assert_eq!(dmg, -2);
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
        assert_eq!(dmg, 0);
        assert_eq!(g.queue.len(), 4);
        assert_eq!(g.queue[0].amount, 2);

        let dmg = g.hard_drop(0, 0);
        assert_eq!(dmg, -2);
        assert_eq!(g.queue.len(), 3);
        assert_eq!(g.queue[0].amount, 1);

        g.update(100);

        let dmg = g.hard_drop(0, 0);
        assert_eq!(dmg, -3);
        assert_eq!(g.queue.len(), 1);
        assert_eq!(g.queue[0].amount, 3);
        assert_eq!(g.queue[0].delay, 50);

        let dmg = g.hard_drop(4, 10);
        assert_eq!(dmg, 7);
        assert_eq!(g.queue.len(), 0);
    }
}