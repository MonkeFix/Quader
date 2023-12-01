use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;
use rand::prelude::*;
use crate::piece::{Piece, PieceType};
use crate::rng_manager::RngManager;

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
    fn get_queue_size(&self) -> usize { 5 }
    fn init(&mut self) -> Vec<Piece>;
    fn next(&mut self) -> Piece;
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
    rng: Rc<RefCell<RngManager>>,
}

impl PieceGeneratorFullRandom {

    pub fn new(rng: &Rc<RefCell<RngManager>>) -> Self {
        Self {
            rng: Rc::clone(rng)
        }
    }

    fn rng(&mut self) -> PieceType {
        *AVAILABLE_PIECES.choose(&mut self.rng.borrow_mut().rng).unwrap()
    }
}

impl PieceGenerator for PieceGeneratorFullRandom {
    fn init(&mut self) -> Vec<Piece> {
        (0..self.get_queue_size())
            .map(|_i| Piece::new(self.rng()))
            .collect()
    }

    fn next(&mut self) -> Piece {
        Piece::new(self.rng())
    }
}

pub struct PieceGeneratorBag7 {
    rng: Rc<RefCell<RngManager>>,
    queue: VecDeque<Piece>
}

impl PieceGeneratorBag7 {
    pub fn new(rng: &Rc<RefCell<RngManager>>) -> Self {
        Self {
            rng: Rc::clone(rng),
            queue: VecDeque::new()
        }
    }

    fn generate_bag(&mut self) -> Vec<PieceType> {
        let mut types = AVAILABLE_PIECES;
        types.shuffle(&mut self.rng.borrow_mut().rng);
        types.to_vec()
    }

    fn enqueue_range(&mut self, types: &[PieceType]) {
        for &t in types {
            self.queue.push_back(Piece::new(t));
        }
    }
}

impl PieceGenerator for PieceGeneratorBag7 {
    fn init(&mut self) -> Vec<Piece> {
        let bag = self.generate_bag();
        let bag2 = self.generate_bag();

        self.queue = VecDeque::from(
            (self.get_queue_size()..BAG_SIZE)
            .map(|i| Piece::new(bag[i]))
            .collect::<Vec<Piece>>()
        );

        self.enqueue_range(&bag2);

        bag.into_iter()
            .take(self.get_queue_size())
            .map(Piece::new)
            .collect()
    }

    fn next(&mut self) -> Piece {
        let p = self.queue.pop_back().expect("The queue must not be empty");

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