/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENCE file in the repository root for full licence text.
 */

use std::sync::{Arc, RwLock};
use tokio::sync::mpsc::{channel, Receiver, Sender};
use tokio::sync::mpsc::error::TryRecvError;
use quader_engine::board::Board;
use quader_engine::board_command::{BoardCommand, BoardMoveDir};
use quader_engine::game_settings::GameSettings;
use quader_engine::piece::RotationDirection;
use quader_engine::piece_mgr::UpdateErrorReason;
use quader_engine::replays::{HardDropInfo, MoveResult};
use quader_engine::time_mgr::TimeMgr;
use quader_engine::wall_kick_data::WallKickData;

pub struct Lobby {
    player_list: Vec<usize>
}

impl Lobby {
    pub fn new(creator: usize) -> Self {
        Self {
            player_list: vec![creator]
        }
    }
}

pub struct BoardInterface {
    send: Sender<BoardCommand>,
    recv: Receiver<Result<MoveResult, UpdateErrorReason>>
}

impl BoardInterface {
    pub fn new(game_settings: GameSettings, wkd: Arc<WallKickData>, seed: u64) -> Self {
        let (board_send, recv) = channel(100);
        let (send, board_recv) = channel(100);
        //std::thread::spawn(move || run(board_recv, board_send, game_settings, wkd, seed));

        tokio::task::spawn(run(board_recv, board_send, game_settings, wkd, seed));

        Self { send, recv }
    }

    pub async fn move_left(&self, delta: u32) {
        self.send.send(BoardCommand::Move(BoardMoveDir::Left, delta)).await.ok();
    }

    pub async fn move_right(&self, delta: u32) {
        self.send.send(BoardCommand::Move(BoardMoveDir::Right, delta)).await.ok();
    }

    pub async fn soft_drop(&self, delta: u32) {
        self.send.send(BoardCommand::SoftDrop(delta)).await.ok();
    }

    pub async fn rotate(&self, rotation_direction: RotationDirection) {
        self.send.send(BoardCommand::Rotate(rotation_direction)).await.ok();
    }

    pub async fn update(&self, dt: f32) {
        self.send.send(BoardCommand::Update(dt)).await.ok();
    }

    pub async fn hold_piece(&self) {
        self.send.send(BoardCommand::HoldPiece).await.ok();
    }

    pub async fn hard_drop(&self) {
        self.send.send(BoardCommand::HardDrop).await.ok();
    }

    pub async fn request_layout(&self) {
        self.send.send(BoardCommand::RequestBoardLayout).await.ok();
    }

    pub async fn send_garbage(&self, amount: u32, messiness: u32) {
        self.send.send(BoardCommand::SendGarbage(amount, messiness)).await.ok();
    }

    pub async fn block_recv_hard_drop(&mut self) -> Option<Result<MoveResult, UpdateErrorReason>> {
        self.recv.recv().await
    }

    pub fn poll_recv_hard_drop(&mut self) -> Result<MoveResult, UpdateErrorReason> {
        let tr = self.recv.try_recv();
        tr.unwrap_or_else(|err| Err(match err {
            TryRecvError::Empty => UpdateErrorReason::BoardDisabled,
            TryRecvError::Disconnected => UpdateErrorReason::BoardDead
        }))
    }
}

async fn run(
    mut recv: Receiver<BoardCommand>,
    send: Sender<Result<MoveResult, UpdateErrorReason>>,
    game_settings: GameSettings,
    wkd: Arc<WallKickData>,
    seed: u64
) {
    let time_mgr = TimeMgr::new();
    let mut board = Board::new(game_settings, wkd, seed);

    while !board.is_dead {
        match recv.recv().await {
            Some(cmd) => {
                let res = match cmd {
                    BoardCommand::Move(dir, dt) => {
                        match dir {
                            BoardMoveDir::Left => { board.move_left(dt); None }
                            BoardMoveDir::Right => { board.move_right(dt); None }
                        }
                    }
                    BoardCommand::Rotate(dir) => { board.rotate(dir); None }
                    BoardCommand::HardDrop => { Some(board.hard_drop()) }
                    BoardCommand::SoftDrop(dt) => { board.soft_drop(dt); None }
                    BoardCommand::SendGarbage(amount, messiness) => { board.push_garbage(amount, messiness); None }
                    BoardCommand::Attack(damage) => { board.attack(damage); None }
                    BoardCommand::Update(dt) => { board.update(&time_mgr) }
                    BoardCommand::HoldPiece => { board.hold_piece(); None }
                    BoardCommand::RequestBoardLayout => { None }
                };

                if let Some(hd) = res {
                    send.send(hd).await.ok();
                }
            },
            None => { return; }
        }
    }
}