use rand::{Rng, SeedableRng};
use rand::distributions::Standard;
use rand::distributions::uniform::{SampleRange, SampleUniform};
use rand::prelude::*;
use rand_chacha::ChaCha8Rng;

#[derive(Debug, Clone)]
pub struct RngManager {
    rng: ChaCha8Rng,
    seed: u64
}

impl RngManager {
    pub fn new(seed: u64) -> Self {
        let rng = SeedableRng::seed_from_u64(seed);
        Self {
            rng,
            seed
        }
    }

    pub fn set_seed(&mut self, seed: u64) {
        self.rng = SeedableRng::seed_from_u64(seed);
        self.seed = seed;
    }

    pub fn get_seed(&self) -> u64 {
        self.seed
    }

    pub fn gen<T>(&mut self) -> T
        where Standard: Distribution<T>  {
        self.rng.gen()
    }

    pub fn gen_range<T, R>(&mut self, range: R) -> T
        where
            T: SampleUniform,
            R: SampleRange<T> {
        self.rng.gen_range(range)
    }
}
