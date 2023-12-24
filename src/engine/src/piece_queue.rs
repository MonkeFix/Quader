use std::collections::VecDeque;
use crate::piece::PieceType;
use crate::piece_generators::{PieceGenerator, PieceGeneratorBag7};

#[derive(Debug)]
pub struct PieceQueue {
    pub queue: VecDeque<PieceType>,
    piece_generator: Box<dyn PieceGenerator>,
    next_piece: PieceType
}

impl PieceQueue {
    pub fn new(seed: u64) -> Self {

        let mut piece_generator = Box::new(PieceGeneratorBag7::new(seed));
        let queue = piece_generator.init();

        Self {
            queue,
            piece_generator,
            next_piece: PieceType::Pixel
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
}