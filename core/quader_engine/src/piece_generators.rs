use std::collections::VecDeque;
use rand::prelude::*;
use rand::rngs::ThreadRng;
use crate::piece::{Piece, PieceType};

pub const AVAILABLE_PIECES: [PieceType; 7] = [
    PieceType::S,
    PieceType::Z,
    PieceType::L,
    PieceType::O,
    PieceType::J,
    PieceType::I,
    PieceType::T
];

pub trait PieceGenerator {
    fn get_queue_size(&self) -> usize { 5 }
    fn init(&mut self) -> Vec<Box<Piece>>;
    fn get(&mut self) -> Piece;
}

pub struct PieceGeneratorFullRandom {
    rng: ThreadRng,
}

impl PieceGeneratorFullRandom {

    pub fn new() -> Self {
        Self {
            rng: thread_rng()
        }
    }

    fn rng(&mut self) -> &PieceType {
        AVAILABLE_PIECES.choose(&mut self.rng).unwrap()
    }
}

impl Default for PieceGeneratorFullRandom {
    fn default() -> Self {
        PieceGeneratorFullRandom::new()
    }
}

impl PieceGenerator for PieceGeneratorFullRandom {
    fn init(&mut self) -> Vec<Box<Piece>> {
        (0..self.get_queue_size())
            .map(|_i| Box::new(Piece::new(*self.rng())))
            .collect()
    }

    fn get(&mut self) -> Piece {
        Piece::new(*self.rng())
    }
}

pub struct PieceGeneratorBag7 {
    rng: ThreadRng,
    queue: VecDeque<Piece>
}

impl PieceGeneratorBag7 {
    pub fn new() -> Self {
        Self {
            rng: thread_rng(),
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
            self.queue.push_back(Piece::new(t));
        }
    }
}

impl Default for PieceGeneratorBag7 {
    fn default() -> Self {
        PieceGeneratorBag7::new()
    }
}

impl PieceGenerator for PieceGeneratorBag7 {
    fn init(&mut self) -> Vec<Box<Piece>> {
        self.queue.clear();

        let mut bag = self.generate_bag();
        let bag2 = self.generate_bag();

        let result = bag.clone()
            .into_iter()
            .take(self.get_queue_size())
            .map(|p| Box::new(Piece::new(p)))
            .collect();

        for i in self.get_queue_size()..7 {
            let piece = bag.remove(i);
            self.queue.push_back(Piece::new(piece));
        }

        self.enqueue_range(&bag2);

        result
    }

    fn get(&mut self) -> Piece {
        let p = self.queue.pop_back().expect("The queue must not be empty");

        if self.queue.len() <= self.get_queue_size() {
            let bag = self.generate_bag();
            self.enqueue_range(&bag);
        }

        p
    }
}