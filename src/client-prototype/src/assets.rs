use std::collections::HashMap;
use macroquad::math::Rect;
use macroquad::prelude::{Font, load_texture, load_ttf_font, Texture2D};
use quader_engine::cell_holder::CellType;

pub const CELL_SIZE: f32 = 32.0;


fn create_cell_rects() -> HashMap<CellType, Rect> {
    let mut result = HashMap::new();
    result.insert(CellType::Z, Rect::new(CELL_SIZE * 0., 0., CELL_SIZE, CELL_SIZE));
    result.insert(CellType::L, Rect::new(CELL_SIZE * 1., 0., CELL_SIZE, CELL_SIZE));
    result.insert(CellType::O, Rect::new(CELL_SIZE * 2., 0., CELL_SIZE, CELL_SIZE));
    result.insert(CellType::S, Rect::new(CELL_SIZE * 3., 0., CELL_SIZE, CELL_SIZE));
    result.insert(CellType::I, Rect::new(CELL_SIZE * 4., 0., CELL_SIZE, CELL_SIZE));
    result.insert(CellType::J, Rect::new(CELL_SIZE * 5., 0., CELL_SIZE, CELL_SIZE));
    result.insert(CellType::T, Rect::new(CELL_SIZE * 6., 0., CELL_SIZE, CELL_SIZE));
    result.insert(CellType::Ghost, Rect::new(CELL_SIZE * 7., 0., CELL_SIZE, CELL_SIZE));
    result.insert(CellType::Solid, Rect::new(CELL_SIZE * 8., 0., CELL_SIZE, CELL_SIZE));
    result.insert(CellType::Garbage, Rect::new(CELL_SIZE * 9., 0., CELL_SIZE, CELL_SIZE));

    result
}


pub struct Assets {
    pub texture_atlas: Texture2D,
    pub cell_rects: HashMap<CellType, Rect>,
    pub board_tex: Texture2D,
    pub font: Font
}

impl Assets {
    pub async fn load() -> Self {
        let texture_atlas = load_texture("assets/skins/default_3.png").await.unwrap();
        let board_tex = load_texture("assets/skins/board_default.png").await.unwrap();
        let font = load_ttf_font("assets/fonts/FiraCode-Regular.ttf").await.unwrap();

        Self {
            texture_atlas,
            board_tex,
            font,
            cell_rects: create_cell_rects()
        }
    }

    pub fn source_rect(&self, cell_type: &CellType) -> Rect {
        self.cell_rects[cell_type].clone()
    }
}