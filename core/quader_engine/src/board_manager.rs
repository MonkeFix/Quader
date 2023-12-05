use std::collections::HashMap;
use std::sync::{Arc, mpsc, Mutex, RwLock};
use std::sync::mpsc::{Receiver, Sender};
use std::thread;
use uuid::Uuid;
use crate::board::{BoardOld};
use crate::board_command::{BoardCommand, BoardCommandType, BoardMessage, BoardMoveDir};
use crate::game_settings::BoardSettings;
use crate::piece::PieceType;


struct RwBoard {
    board: RwLock<Box<BoardOld>>,
    sender: Sender<BoardMessage>
}

type Boards = Arc<Mutex<HashMap<Uuid, RwBoard>>>;

pub struct BoardManager {
    boards: Boards,
    board_settings: BoardSettings
}

impl BoardManager {
    pub fn new(board_settings: BoardSettings) -> Self {
        Self {
            boards: Boards::default(),
            board_settings
        }
    }

    pub fn add_board(&mut self) -> (String, Receiver<BoardMessage>) {
        let uuid = Uuid::new_v4();

        let mut board = BoardOld::new(&self.board_settings);
        board.create_piece(PieceType::J);
        let rw_board = RwLock::new(Box::new(board));
        let (sender, receiver) = mpsc::channel();

        self.boards.lock().unwrap().insert(uuid, RwBoard {
            board: rw_board, sender
        });

        (uuid.to_string(), receiver)
    }

    pub fn send_command(&self, uuid: &str, cmd: BoardCommand) {
        let boards = self.boards.lock().unwrap();
        let uuid = Uuid::parse_str(uuid).unwrap();
        let rw_board = boards.get(&uuid)
            .unwrap_or_else(|| panic!("Board with uuid '{}' doesn't exist", &uuid));

        let mut mutex = rw_board.board.write();

        let (board, sender) = (mutex.as_mut().unwrap(), rw_board.sender.clone());

        self.parse_cmd(board, &cmd);

        sender
            .send(BoardMessage::BoardUpdated)
            .expect("TODO: panic message");

        /*thread::spawn(move || {
            println!("sending");
            for i in 0..10 {
                if let Err(_) = sender.send(BoardMessage::BoardUpdated) {
                    println!("receiver dropped");
                    return;
                }
            }
        });*/
    }

    pub fn get_board_count(&self) -> usize {
        self.boards.lock().unwrap().len()
    }

    pub fn broadcast(&self, cmd: &BoardCommand) {
        self.for_each(|b: &mut BoardOld| {
            self.parse_cmd(b, cmd);
        });
    }

    pub fn update(&self, dt: f32) {
        self.for_each(|b: &mut BoardOld| {
            b.update(dt);
        });
    }

    fn for_each<F>(&self, action: F)
        where F: Fn(&mut BoardOld) {
        let boards = self.boards.lock().unwrap();

        for lock in boards.iter() {
            let mut mutex = lock.1.board.write();
            let board = mutex.as_mut().unwrap();
            action(board);
        }
    }

    fn parse_cmd(&self, board: &mut BoardOld, cmd: &BoardCommand) {
        match cmd.get_type() {
            BoardCommandType::Move(dir, delta) => {
                match dir {
                    BoardMoveDir::Left => board.move_left(*delta),
                    BoardMoveDir::Right => board.move_right(*delta)
                }
            },
            BoardCommandType::Rotate(dir) => board.rotate(dir),
            BoardCommandType::HardDrop => board.hard_drop(),
            BoardCommandType::SoftDrop(delta) => board.soft_drop(*delta),
            BoardCommandType::SendGarbage(amount, messiness) => board.send_garbage(*amount, *messiness),
        }
    }
}