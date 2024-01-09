/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENCE file in the repository root for full licence text.
 */

use std::collections::VecDeque;
use crate::piece::PieceType;
use crate::piece_generators::{PieceGenerator, PieceGeneratorBag7};

#[derive(Debug)]
pub struct PieceQueue {
    pub queue: VecDeque<PieceType>,
    piece_generator: Box<PieceGeneratorBag7>,
    next_piece: PieceType,
    seed: u64
}

impl PieceQueue {
    pub fn new(seed: u64) -> Self {

        let mut piece_generator = Box::new(PieceGeneratorBag7::new(seed));
        let queue = piece_generator.init();

        Self {
            queue,
            piece_generator,
            next_piece: PieceType::Pixel,
            seed
        }
    }

    pub fn next(&mut self) -> PieceType {
        self.set_piece()
    }

    fn set_piece(&mut self) -> PieceType {
        self.next_piece = self.piece_generator.next();
        let next = self.queue.pop_front().unwrap();

        self.queue.push_back(self.next_piece);

        next
    }

    pub fn reset(&mut self, new_seed: Option<u64>) {

        let seed = new_seed.unwrap_or_else(|| self.seed);

        let mut piece_generator = Box::new(PieceGeneratorBag7::new(seed));
        let queue = piece_generator.init();

        self.queue = queue;
        self.piece_generator = piece_generator;
        self.next_piece = PieceType::Pixel;
        self.seed = seed;
    }
}