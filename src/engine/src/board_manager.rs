/*use std::cell::{RefCell};
use std::collections::HashMap;
use std::ops::DerefMut;
use std::rc::Rc;
use std::sync::{Arc, mpsc, Mutex};
use std::sync::mpsc::{channel, Receiver, RecvError, Sender, TryRecvError};
use rand::Rng;
use uuid::Uuid;
use crate::board::{Board};
use crate::board_command::{BoardCommand, BoardMessage, BoardMoveDir};
use crate::cell_holder::Row;
use crate::game_settings::GameSettings;
use crate::piece::RotationDirection;
use crate::replays::HardDropInfo;
use crate::rng_manager::RngManager;
use crate::time_mgr::TimeMgr;
use crate::wall_kick_data::WallKickData;

struct RwBoard {
    board: Rc<RefCell<Board>>,
    sender: Sender<BoardMessage>,
}

pub struct BoardInterface {
    send: Sender<BoardCommand>,
    recv: Receiver<HardDropInfo>
}

impl BoardInterface {
    pub fn new(game_settings: GameSettings, wkd: Arc<WallKickData>, seed: u64) -> Self {
        let (board_send, recv) = channel();
        let (send, board_recv) = channel();
        std::thread::spawn(move || run(board_recv, board_send, game_settings, wkd, seed));

        Self { send, recv }
    }

    pub fn move_left(&self, delta: u32) {
        self.send.send(BoardCommand::Move(BoardMoveDir::Left, delta)).ok();
    }

    pub fn move_right(&self, delta: u32) {
        self.send.send(BoardCommand::Move(BoardMoveDir::Right, delta)).ok();
    }

    pub fn soft_drop(&self, delta: u32) {
        self.send.send(BoardCommand::SoftDrop(delta)).ok();
    }

    pub fn rotate(&self, rotation_direction: RotationDirection) {
        self.send.send(BoardCommand::Rotate(rotation_direction)).ok();
    }

    pub fn update(&self, dt: f32) {
        self.send.send(BoardCommand::Update(dt)).ok();
    }

    pub fn hold_piece(&self) {
        self.send.send(BoardCommand::HoldPiece).ok();
    }

    pub fn hard_drop(&self) {
        self.send.send(BoardCommand::HardDrop).ok();
    }

    pub fn request_layout(&self) {
        self.send.send(BoardCommand::RequestBoardLayout).ok();
    }

    pub fn send_garbage(&self, amount: u32, messiness: u32) {
        self.send.send(BoardCommand::SendGarbage(amount, messiness)).ok();
    }

    pub fn block_recv_hard_drop(&self) -> Option<HardDropInfo> {
        self.recv.recv().ok()
    }

    /// Returns `true` if the player is dead.
    pub fn poll_recv_hard_drop(&self) -> Result<HardDropInfo, bool> {
        self.recv.try_recv().map_err(|e| match e {
            TryRecvError::Empty => false,
            TryRecvError::Disconnected => true,
        })
    }
}

fn run(
    recv: Receiver<BoardCommand>,
    send: Sender<HardDropInfo>,
    game_settings: GameSettings,
    wkd: Arc<WallKickData>,
    seed: u64
) {
    let mut board = Board::new(game_settings, wkd, seed);

    while !board.is_dead {
        match recv.recv() {
            Ok(cmd) => {
                let res = board.exec_cmd(&cmd);
                if let Some(hd) = res {
                    send.send(hd).ok();
                }
            },
            Err(_) => { return; }
        }
    }
}

type Boards = Arc<Mutex<HashMap<Uuid, RwBoard>>>;

pub struct BoardManager {
    boards: Boards,
    game_settings: GameSettings,
    rng_manager: RngManager,
    time_mgr: TimeMgr,
    wkd: Arc<WallKickData>,
    pub bi: BoardInterface
}

impl BoardManager {
    pub fn new(game_settings: GameSettings) -> Self {
        let mut rng = rand::thread_rng();
        let seed: u64 = rng.gen();
        let wkd = Arc::new(WallKickData::new(game_settings.wall_kick_data_mode));
        let wkd2 = Arc::clone(&wkd);

        Self {
            boards: Boards::default(),
            game_settings,
            rng_manager: RngManager::new(seed),
            time_mgr: TimeMgr::new(),
            wkd,
            bi: BoardInterface::new(game_settings, wkd2, seed)
        }
    }

    pub fn add_board(&mut self) -> (String, Receiver<BoardMessage>, Rc<RefCell<Board>>) {
        let uuid = Uuid::new_v4();

        let board = RefCell::new(Board::new(self.game_settings, Arc::clone(&self.wkd), self.rng_manager.get_seed()));
        let rw_board = Rc::new(board);

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
            BoardCommand::Move(dir, delta) => {
                match dir {
                    BoardMoveDir::Left => self.bi.move_left(delta),
                    BoardMoveDir::Right => self.bi.move_right(delta)
                }
            }
            BoardCommand::Rotate(dir) => self.bi.rotate(dir),
            BoardCommand::HardDrop => self.bi.hard_drop(),
            BoardCommand::SoftDrop(delta) => self.bi.soft_drop(delta),
            BoardCommand::SendGarbage(amount, messiness) => self.bi.send_garbage(amount, messiness),
            BoardCommand::Update(dt) => {
                self.time_mgr.update(dt);
                //self.bi.update(dt);
            }
            BoardCommand::HoldPiece => self.bi.hold_piece(),
            BoardCommand::RequestBoardLayout => self.bi.request_layout()
        }

        /*match cmd {
            BoardCommand::Update(dt) => self.time_mgr.update(dt),
            _ => {
                sender
                    .send(BoardMessage::BoardUpdated)
                    .expect("TODO: panic message");
            }
        };*/

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
}*/