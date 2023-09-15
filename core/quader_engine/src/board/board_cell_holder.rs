pub const BOARD_WIDTH: usize = 10;
pub const BOARD_HEIGHT: usize = 80;

pub enum CellType {
    None,
    I, O, T, L, J, S, Z,
    Garbage,
    Solid,
    Failing
}

struct BoardCellHolder {
    layout: [[CellType; BOARD_WIDTH]; BOARD_HEIGHT]
}
/*
impl<R: Row> BoardCellHolder<R> {
    pub fn new() -> Self {
        BoardCellHolder {
            layout: [; BOARD_HEIGHT].into()
        }
    }
}
*/