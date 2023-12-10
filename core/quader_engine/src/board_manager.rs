use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::ops::DerefMut;
use std::rc::Rc;
use std::sync::{Arc, mpsc, Mutex, RwLock};
use std::sync::mpsc::{Receiver, Sender};
use std::thread;
use uuid::Uuid;
use crate::board::{Board, BoardEntity};
use crate::board_command::{BoardCommand, BoardCommandType, BoardMessage, BoardMoveDir};
use crate::cell_holder::CellHolder;
use crate::game_settings::{BoardSettings, GameSettings};
use crate::piece::PieceType;
use crate::piece_mgr::PieceMgr;
use crate::wall_kick_data::WallKickData;

struct RwBoard {
    board: Rc<RefCell<Board>>,
    sender: Sender<BoardMessage>
}

type Boards = Arc<Mutex<HashMap<Uuid, RwBoard>>>;

pub struct BoardManager {
    boards: Boards,
    game_settings: GameSettings
}

impl BoardManager {
    pub fn new(game_settings: GameSettings) -> Self {
        Self {
            boards: Boards::default(),
            game_settings
        }
    }

    pub fn add_board(&mut self) -> (String, Receiver<BoardMessage>) {
        let uuid = Uuid::new_v4();

        let board = RefCell::new(Board::new(self.game_settings));
        let rw_board = Rc::new(board);

        rw_board.borrow_mut().add_component(CellHolder::new(self.game_settings.get_board()));
        rw_board.borrow_mut().add_component(PieceMgr::new(&self.game_settings, Rc::clone(&rw_board)));

        //board.create_piece(PieceType::J);

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

        //let mut mutex = rw_board.board.write();

        let mut binding = rw_board.board.borrow_mut();
        let (board, sender) = (binding.deref_mut(), rw_board.sender.clone());

        board.exec_cmd(&cmd);
        // self.parse_cmd(board, &cmd);

        /*if cmd.get_type() != BoardCommandType::Update {
            sender
                .send(BoardMessage::BoardUpdated)
                .expect("TODO: panic message");
        }*/

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

    /*pub fn broadcast(&self, cmd: &BoardCommand) {
        self.for_each(|b: &mut Rc<Board>| {
            self.parse_cmd(b, cmd);
        });
    }*/

    /*pub fn update(&self, dt: f32) {
        /*self.for_each(|b: &mut Rc<Board>| {
            b.update(dt);
        });*/
        let boards = self.boards.lock().unwrap();

        for b in boards.iter() {
            b.1.board.borrow_mut().deref_mut().update(dt);
        }
    }*/

    /*fn for_each<F>(&self, action: F)
        where F: Fn(&mut Rc<Board>) {
        let boards = self.boards.lock().unwrap();

        for lock in boards.iter() {
            let mut board = lock.1.board.borrow_mut().deref_mut();
            action(board);
        }
    }*/

    /*fn parse_cmd(&self, board: &mut Board, cmd: &BoardCommand) {
        match cmd.get_type() {
            BoardCommandType::Move(dir, delta) => {
                match dir {
                    BoardMoveDir::Left => board.move_left(*delta),
                    BoardMoveDir::Right => board.move_right(*delta)
                }
            },
            BoardCommandType::Rotate(dir) => board.rotate(&WallKickData::default(), dir),
            BoardCommandType::HardDrop => board.hard_drop(),
            BoardCommandType::SoftDrop(delta) => board.soft_drop(*delta),
            BoardCommandType::SendGarbage(amount, messiness) => board.send_garbage(*amount, *messiness),
        }
    }*/
}