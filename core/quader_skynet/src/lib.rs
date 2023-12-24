use libtetris::Board;

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

pub fn aa() {
    let mut board = Board::new();
    board.add_next_piece(libtetris::Piece::I);
    let b = Box::new(cold_clear::Interface::launch(
        board,
        cold_clear::Options::default(),
        cold_clear::evaluation::Standard::default(),
        None
    ));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
