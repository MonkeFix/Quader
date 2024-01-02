extern crate core;

pub mod board;
pub mod primitives;
pub mod piece;
pub mod wall_kick_data;
pub mod cell_holder;
pub mod piece_generators;
pub mod rng_manager;
pub mod board_manager;
pub mod board_command;
pub mod damage_calculation;
pub mod game_settings;
pub mod replays;
pub mod scoring;
mod gravity_mgr;
mod time_mgr;
pub mod piece_mgr;
mod command_dispatcher;
pub mod utils;
pub mod piece_points;
mod piece_queue;
pub mod garbage_mgr;

pub fn add(left: usize, right: usize) -> usize {
    left + right
}


pub fn test() {

}
