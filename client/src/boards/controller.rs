use macroquad::prelude::*;
use quader_engine::prelude::*;
use quader_skynet::{BotBoard, BotSettings};
use std::net::TcpStream;
use url::Url;

use tungstenite::{connect, stream::MaybeTlsStream, Message, WebSocket};

use super::ControllerSettings;

pub trait Controller {
    fn update(&mut self, time_mgr: &TimeMgr) -> Option<Result<MoveResult, BoardErrorReason>>;
    fn reset(&mut self, new_seed: Option<u64>);
    fn board(&self) -> &Board;
    fn board_mut(&mut self) -> &mut Board;
    fn disable(&mut self);
}

pub struct ControllerBot {
    pub bot_board: Box<BotBoard>,
}

impl ControllerBot {
    pub fn new(settings: ControllerSettings, target_pps: f32) -> Self {
        Self {
            bot_board: Box::new(BotBoard::new(
                settings.game_settings,
                settings.wkd,
                settings.seed,
                BotSettings { target_pps },
            )),
        }
    }
}

impl Controller for ControllerBot {
    fn update(&mut self, time_mgr: &TimeMgr) -> Option<Result<MoveResult, BoardErrorReason>> {
        self.bot_board.update(time_mgr)
    }

    fn reset(&mut self, new_seed: Option<u64>) {
        self.bot_board.reset(new_seed);
    }

    fn board(&self) -> &Board {
        &self.bot_board.engine_board
    }

    fn board_mut(&mut self) -> &mut Board {
        &mut self.bot_board.engine_board
    }

    fn disable(&mut self) {
        self.bot_board.disable();
    }
}

struct PieceMover {
    elapsed: f32,
    #[allow(dead_code)]
    arr: f32,
    das: f32,
    sdf: u32,
    is_left_down: bool,
    is_right_down: bool,
}

impl PieceMover {
    pub fn move_left(&self, board: &mut Board) {
        board.move_left(1);
    }

    pub fn move_right(&self, board: &mut Board) {
        board.move_right(1);
    }

    pub fn reset(&mut self) {
        self.elapsed = 0.0;
        self.is_left_down = false;
        self.is_right_down = false;
    }
}

pub struct ControllerPlayer {
    pub board: Board,
    piece_mover: PieceMover,
}

impl ControllerPlayer {
    pub fn new(settings: ControllerSettings) -> Self {
        let board = Board::new(settings.game_settings, settings.wkd, settings.seed);

        Self {
            board,
            piece_mover: PieceMover {
                elapsed: 0.0,
                arr: 0.0,
                das: 128.0,
                sdf: u32::MAX,
                is_left_down: false,
                is_right_down: false,
            },
        }
    }
}

impl Controller for ControllerPlayer {
    fn update(&mut self, time_mgr: &TimeMgr) -> Option<Result<MoveResult, BoardErrorReason>> {
        if !self.board.is_enabled() {
            return None;
        }

        let elapsed = time_mgr.last_dt * 1000.0; // converting to milliseconds
        let mut result = None;

        if is_key_pressed(KeyCode::Left) {
            self.piece_mover.move_left(&mut self.board);
        }
        if is_key_down(KeyCode::Left) {
            self.piece_mover.is_left_down = true;
            self.piece_mover.elapsed += elapsed;
        }
        if is_key_released(KeyCode::Left) {
            self.piece_mover.is_left_down = false;
            self.piece_mover.elapsed = 0.0;
        }

        if is_key_pressed(KeyCode::Right) {
            self.piece_mover.move_right(&mut self.board);
        }
        if is_key_down(KeyCode::Right) {
            self.piece_mover.is_right_down = true;
            self.piece_mover.elapsed += elapsed;
        }
        if is_key_released(KeyCode::Right) {
            self.piece_mover.is_right_down = false;
            self.piece_mover.elapsed = 0.0;
        }

        if self.piece_mover.elapsed >= self.piece_mover.das {
            let moves = 10;

            for _ in 0..moves {
                if self.piece_mover.is_left_down {
                    self.piece_mover.move_left(&mut self.board);
                }
                if self.piece_mover.is_right_down {
                    self.piece_mover.move_right(&mut self.board);
                }
            }
        }

        if is_key_down(KeyCode::Down) {
            self.board.soft_drop(self.piece_mover.sdf);
        }
        if is_key_pressed(KeyCode::Space) {
            result = Some(self.board.hard_drop());
        }
        if is_key_pressed(KeyCode::Z) {
            self.board.rotate(RotationDirection::CounterClockwise);
        }
        if is_key_pressed(KeyCode::X) {
            self.board.rotate(RotationDirection::Clockwise);
        }
        if is_key_pressed(KeyCode::F) {
            self.board.rotate(RotationDirection::Deg180);
        }
        if is_key_pressed(KeyCode::C) {
            self.board.try_hold_piece();
        }

        if is_key_pressed(KeyCode::T) {
            let mut rng = RngManager::from_entropy();
            self.board.attack(rng.gen_range(0..6));
        }

        if let Some(res) = self.board.update(time_mgr) {
            result = Some(res);
        }

        result
    }

    fn reset(&mut self, new_seed: Option<u64>) {
        self.board.reset(new_seed);
        self.piece_mover.reset();
    }

    fn board(&self) -> &Board {
        &self.board
    }

    fn board_mut(&mut self) -> &mut Board {
        &mut self.board
    }

    fn disable(&mut self) {
        self.board.disable();
    }
}

#[allow(dead_code)]
pub struct ControllerRemote {
    socket: WebSocket<MaybeTlsStream<TcpStream>>,
}

#[allow(dead_code)]
impl ControllerRemote {
    pub fn new(connection_uri: &str) -> Self {
        let url = Url::parse(connection_uri).unwrap();

        let (mut socket, response) = connect(url.as_str()).expect("Can't connect");

        println!("Connected to the server");
        println!("Response HTTP code: {}", response.status());
        println!("Response contains the following headers:");

        for (ref header, _value) in response.headers() {
            println!("* {}", header);
        }

        socket.send(Message::Text("Hello WS!".into())).unwrap();

        let msg = socket.read().expect("Error reading message");
        println!("Received: {}", msg);

        match msg {
            Message::Text(_content) => {
                //serde_json::from_str(&content);
            }
            Message::Close(_close_frame) => {}
            Message::Ping(_) | Message::Pong(_) | Message::Frame(_) | Message::Binary(_) => {}
        }

        Self { socket }
    }
}

impl Controller for ControllerRemote {
    fn update(&mut self, _time_mgr: &TimeMgr) -> Option<Result<MoveResult, BoardErrorReason>> {
        unimplemented!();
    }

    fn reset(&mut self, _new_seed: Option<u64>) {
        unimplemented!();
    }

    fn board(&self) -> &Board {
        unimplemented!();
    }

    fn board_mut(&mut self) -> &mut Board {
        unimplemented!()
    }

    fn disable(&mut self) {
        unimplemented!()
    }
}
