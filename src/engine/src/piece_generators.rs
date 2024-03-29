/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full licence text.
 */

use std::collections::VecDeque;
use std::fmt::{Debug, Formatter};
use rand::prelude::*;
use rand_chacha::ChaCha8Rng;
use crate::piece::{PieceType};

pub const AVAILABLE_PIECES: [PieceType; 7] = [
    PieceType::S,
    PieceType::Z,
    PieceType::L,
    PieceType::O,
    PieceType::J,
    PieceType::I,
    PieceType::T
];

pub const BAG_SIZE: usize = AVAILABLE_PIECES.len();

pub trait PieceGenerator {
    /// Returns queue size.
    fn get_queue_size(&self) -> usize { 5 }
    /// Creates and returns initial piece queue.
    fn init(&mut self) -> VecDeque<PieceType>;
    /// Generates a new piece.
    fn next(&mut self) -> PieceType;
}

impl Debug for dyn PieceGenerator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "PieceGenerator{{queue_size: {}}}", self.get_queue_size())
    }
}


/*pub enum PieceGeneratorType {
    FullRandom, Bag7
}

pub struct PieceGeneratorFactory;
impl PieceGeneratorFactory {
    pub fn create(rng: Rc<RngManager>, generator_type: PieceGeneratorType) -> Box<dyn PieceGenerator> {
        match generator_type {
            PieceGeneratorType::FullRandom => Box::new(PieceGeneratorFullRandom::new(rng)),
            PieceGeneratorType::Bag7 => Box::new(PieceGeneratorBag7::new(rng))
        }
    }
}*/

pub struct PieceGeneratorFullRandom {
    rng: ChaCha8Rng,
}

impl PieceGeneratorFullRandom {

    pub fn new(seed: u64) -> Self {
        Self {
            rng: SeedableRng::seed_from_u64(seed)
        }
    }

    fn rng(&mut self) -> PieceType {
        *AVAILABLE_PIECES.choose(&mut self.rng).unwrap()
    }
}

impl PieceGenerator for PieceGeneratorFullRandom {
    fn init(&mut self) -> VecDeque<PieceType> {
        (0..self.get_queue_size())
            .map(|_i| self.rng())
            .collect()
    }

    fn next(&mut self) -> PieceType {
        self.rng()
    }
}

#[derive(Debug)]
pub struct PieceGeneratorBag7 {
    rng: ChaCha8Rng,
    queue: VecDeque<PieceType>
}

impl PieceGeneratorBag7 {
    pub fn new(seed: u64) -> Self {
        Self {
            rng: SeedableRng::seed_from_u64(seed),
            queue: VecDeque::new()
        }
    }

    fn generate_bag(&mut self) -> Vec<PieceType> {
        let mut types = AVAILABLE_PIECES;
        types.shuffle(&mut self.rng);
        types.to_vec()
    }

    fn enqueue_range(&mut self, types: &[PieceType]) {
        for &t in types {
            self.queue.push_back(t);
        }
    }
}

impl PieceGenerator for PieceGeneratorBag7 {
    fn init(&mut self) -> VecDeque<PieceType> {
        let bag = self.generate_bag();
        let bag2 = self.generate_bag();

        self.queue = (self.get_queue_size()..BAG_SIZE)
            .map(|i|bag[i])
            .collect::<VecDeque<PieceType>>();

        self.enqueue_range(&bag2);

        let res = bag.into_iter()
            .take(self.get_queue_size())
            .collect();

        res
    }

    fn next(&mut self) -> PieceType {
        let p = self.queue.pop_front().expect("The queue must not be empty");

        if self.queue.len() <= self.get_queue_size() {
            let bag = self.generate_bag();
            self.enqueue_range(&bag);
        }

        p
    }
}

#[cfg(test)]
mod tests {

}