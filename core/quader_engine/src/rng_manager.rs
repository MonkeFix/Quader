use rand::{Rng, SeedableRng};
use rand::distributions::Standard;
use rand::distributions::uniform::{SampleRange, SampleUniform};
use rand::prelude::*;
use rand_chacha::ChaCha8Rng;

pub struct RngManager {
    pub rng: ChaCha8Rng
}

impl Default for RngManager {
    fn default() -> Self {
        Self {
            rng: SeedableRng::from_entropy()
        }
    }
}

impl RngManager {
    pub fn new(seed: u64) -> Self {
        Self {
            rng: SeedableRng::seed_from_u64(seed)
        }
    }

    pub fn set_seed(&mut self, seed: u64) {
        self.rng = SeedableRng::seed_from_u64(seed);
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
