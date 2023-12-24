use std::cell::{RefCell};
use std::collections::HashMap;
use std::ops::DerefMut;
use std::rc::Rc;
use std::sync::{Arc, mpsc, Mutex};
use std::sync::mpsc::{Receiver, Sender};
use rand::Rng;
use uuid::Uuid;
use crate::board::{Board};
use crate::board_command::{BoardCommand, BoardMessage};
use crate::game_settings::GameSettings;
use crate::rng_manager::RngManager;
use crate::time_mgr::TimeMgr;
use crate::wall_kick_data::WallKickData;

struct RwBoard {
    board: Rc<RefCell<Board>>,
    sender: Sender<BoardMessage>,
}

type Boards = Arc<Mutex<HashMap<Uuid, RwBoard>>>;

pub struct BoardManager {
    boards: Boards,
    game_settings: GameSettings,
    rng_manager: RngManager,
    time_mgr: TimeMgr,
    wkd: Arc<WallKickData>
}

impl BoardManager {
    pub fn new(game_settings: GameSettings) -> Self {
        let mut rng = rand::thread_rng();
        let seed: u64 = rng.gen();

        Self {
            boards: Boards::default(),
            game_settings,
            rng_manager: RngManager::new(seed),
            time_mgr: TimeMgr::new(),
            wkd: Arc::new(WallKickData::new(game_settings.wall_kick_data_mode))
        }
    }

    pub fn add_board(&mut self) -> (String, Receiver<BoardMessage>, Rc<RefCell<Board>>) {
        let uuid = Uuid::new_v4();

        let board = RefCell::new(Board::new(self.game_settings, &self.wkd, self.rng_manager.get_seed()));
        let rw_board = Rc::new(board);

        // rw_board.borrow_mut().get_piece_mgr_mut().set_piece(PieceType::I);

        let (sender, receiver) = mpsc::channel();

        let res_board = Rc::clone(&rw_board);

        self.boards.lock().unwrap().insert(uuid, RwBoard {
            board: rw_board, sender
        });

        (uuid.to_string(), receiver, res_board)
    }

    pub fn get_board(&self, uuid: &str) -> Rc<RefCell<Board>> {
        let uuid = Uuid::parse_str(uuid).unwrap();
        Rc::clone(&self.boards.lock().unwrap().get(&uuid).unwrap().board)
    }

    pub fn send_command(&mut self, uuid: &str, cmd: BoardCommand) {
        let boards = self.boards.lock().unwrap();
        let uuid = Uuid::parse_str(uuid).unwrap();
        let rw_board = boards.get(&uuid)
            .unwrap_or_else(|| panic!("Board with uuid '{}' doesn't exist", &uuid));

        //let mut mutex = rw_board.board.write();

        let mut binding = rw_board.board.borrow_mut();
        let (board, sender) = (binding.deref_mut(), rw_board.sender.clone());

        board.exec_cmd(&cmd);

        match cmd {
            BoardCommand::Update(dt) => self.time_mgr.update(dt),
            _ => {
                sender
                    .send(BoardMessage::BoardUpdated)
                    .expect("TODO: panic message");
            }
        };

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