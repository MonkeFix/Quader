use std::rc::Rc;
use quader_engine::board_cell_holder::BoardCellHolder;
use quader_engine::piece::{Piece, PieceType};
use quader_engine::wall_kick_data::WallKickData;


pub struct Debugging {
    wall_kick_data: Rc<WallKickData>,
    cell_container: BoardCellHolder,
    piece: Piece
}

impl Debugging {
    pub fn new() -> Self {
        let wkd = Rc::new(WallKickData::new());

        Debugging {
            cell_container: BoardCellHolder::default(),
            piece: Piece::new(Rc::downgrade(&wkd), PieceType::J),
            wall_kick_data: wkd
        }
    }

    pub fn get_data_mut(&self) -> (&WallKickData, &BoardCellHolder, &Piece) {
        (&self.wall_kick_data, &self.cell_container, &self.piece)
    }
}