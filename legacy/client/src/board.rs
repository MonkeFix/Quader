use std::sync::Arc;
use bevy::a11y::accesskit::NodeBuilder;
use bevy::input::keyboard::KeyboardInput;
use bevy::prelude::*;
use bevy::render::render_resource::{Extent3d, TextureDescriptor, TextureDimension, TextureFormat, TextureUsages};
use bevy::render::view::RenderLayers;
use bevy::sprite::{Anchor, MaterialMesh2dBundle};
use bevy::utils::HashMap;
use bevy_inspector_egui::inspector_options::std_options::EntityDisplay::Id;
use uuid::Uuid;
use quader_engine::board::Board;
use quader_engine::cell_holder::{CellHolder, CellType, Row};
use quader_engine::game_settings::GameSettings;
use quader_engine::piece;
use quader_engine::piece::{Piece, RotationDirection};
use quader_engine::primitives::Point;
use quader_engine::rng_manager::RngManager;
use quader_engine::wall_kick_data::{WallKickData, WallKickDataMode};

#[derive(Component, Debug)]
pub struct Position {
    val: Vec2
}

#[derive(Component, Debug)]
pub struct BoardComponent {
    pub board: Board
}

#[derive(Component, Debug)]
pub struct BoardCellComponent {
    pub cell_type: CellType,
    pub position: Point,
}

#[derive(Component, Debug)]
pub struct BoardCellHolderComponent {
    pub layout: Vec<Row>
}

#[derive(Component, Debug)]
pub struct PieceComponent {
    //pub positions: Vec<Point>
}

#[derive(Bundle, Debug)]
pub struct BoardBundle {
    board: BoardComponent
}

#[derive(Resource, Debug, Default)]
pub struct CellSprites {
    pub atlas: Handle<TextureAtlas>,
    pub sprite_map: HashMap<CellType, TextureAtlasSprite>,
    pub ghost_sprite: TextureAtlasSprite,
}

impl Default for BoardBundle {
    fn default() -> Self {
        Self {
            board: BoardComponent {
                board: Board::new(
                    GameSettings::default(),
                    Arc::new(WallKickData::new(WallKickDataMode::Standard)),
                    RngManager::from_entropy().gen()
                )
            }
        }
    }
}

pub struct BoardPlugin;

impl Plugin for BoardPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, build_system);
        app.add_systems(Update, (keyboard_input_system, update_board));
    }
}

/*fn update(keyboard_input: Res<Button>,
          mut board: Query<(Entity, &mut BoardComponent)>) {
    let board = board.single_mut();
    let board = &board.1.board;


}*/

fn update_board(
    time: Res<Time>,
    mut q: Query<(Entity, &mut BoardComponent)>
) {
    if q.is_empty() {
        return;
    }
    let mut board = q.single_mut();
    let mut board = &mut board.1.board;

    board.update(time.delta_seconds());
}

fn keyboard_input_system(
    commands: Commands,
    keyboard_input: Res<Input<KeyCode>>,
    mut q: Query<(Entity, &mut BoardComponent), With<BoardComponent>>,
    cells_query: Query<(Entity, &BoardCellHolderComponent)>,
    cell_sprites: Res<CellSprites>,
    mut piece_query: Query<(Entity, &mut Transform), With<PieceComponent>>
) {

    if q.is_empty() {
        return;
    }
    if piece_query.is_empty() {
        return;
    }

    let mut piece = piece_query.single_mut();

    let mut board_ent = q.single_mut();
    let entity = board_ent.0;
    let mut board = &mut board_ent.1.board;

    if keyboard_input.just_pressed(KeyCode::Z) {
        if let Some(_) = board.rotate(RotationDirection::CounterClockwise) {

        }

    }
    if keyboard_input.just_pressed(KeyCode::X) {
        if let Some(_) = board.rotate(RotationDirection::Clockwise) {

        }
    }
    if keyboard_input.just_pressed(KeyCode::F) {
        if let Some(_) = board.rotate(RotationDirection::Deg180) {

        }
    }

    if keyboard_input.just_pressed(KeyCode::Left) {
        if board.move_left(1) > 0 {
            piece.1.translation.x -= 32.0;
        }
    }
    if keyboard_input.just_pressed(KeyCode::Right) {
        if board.move_right(1) > 0 {
            piece.1.translation.x += 32.0;
        }
    }
    if keyboard_input.just_pressed(KeyCode::Down) {
        if board.soft_drop(1) > 0 {
            piece.1.translation.y -= 32.0;
        }
    }

    if keyboard_input.just_pressed(KeyCode::C) {
        board.hold_piece();
    }


    if keyboard_input.just_pressed(KeyCode::Space) {
        board.hard_drop().ok();
        rebuild_board_layout(commands, cells_query, &board, cell_sprites);
    }
}

fn rebuild_board_layout(
    mut commands: Commands,
    cells_query: Query<(Entity, &BoardCellHolderComponent)>,
    board: &Board,
    cell_sprites: Res<CellSprites>
) {

    for entity in cells_query.iter() {
        commands.entity(entity.0).despawn_recursive();

        commands.spawn((SpatialBundle {
            transform: Transform::from_xyz(0.0, -247., 0.0),
            ..default()
        }, BoardCellHolderComponent {
            layout: vec![]
        }, Name::new("board-cells"))).with_children(|commands| {
            for (y, row) in board.get_cell_holder().get_layout().iter().rev().enumerate() {
                for (x, cell) in row.iter().enumerate() {

                    // draw cells
                    if *cell != CellType::None {

                        commands.spawn(BoardCellComponent {
                            cell_type: *cell,
                            position: Point::new(x as i32, y as i32)
                        }).insert(SpriteSheetBundle {
                            texture_atlas: cell_sprites.atlas.clone(),
                            sprite: cell_sprites.sprite_map[cell].clone(),
                            transform: Transform::from_xyz(x as f32 * 32.0, y as f32 * 32.0, 1.0),
                            ..default()
                        });

                    }
                }
            }
        });
    }
}

fn build_system(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut texture_atlases: ResMut<Assets<TextureAtlas>>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut cell_sprites: ResMut<CellSprites>
) {

    let mut board_bundle = BoardBundle::default();
    let mut board = &mut board_bundle.board.board;
    let mut piece_mgr = &mut board.piece_mgr;
    let piece = piece_mgr.cur_piece;

    let texture_handle = asset_server.load("skins/default_3.png");

    let atlas = TextureAtlas::from_grid(
        texture_handle.clone(),
        Vec2::new(32., 32.),
        12,
        1,
        None,
        None
    );
    let texture_atlas_handle = texture_atlases.add(atlas);

    let mut sprite = TextureAtlasSprite::new(0);
    sprite.anchor = Anchor::TopLeft;

    let mut grid_sprite = TextureAtlasSprite::new(10);
    grid_sprite.anchor = Anchor::TopLeft;

    let sprite_z = TextureAtlasSprite {
        index: 0,
        anchor: Anchor::TopLeft,
        ..default()
    };
    let sprite_l = TextureAtlasSprite {
        index: 1,
        anchor: Anchor::TopLeft,
        ..default()
    };
    let sprite_o = TextureAtlasSprite {
        index: 2,
        anchor: Anchor::TopLeft,
        ..default()
    };
    let sprite_s = TextureAtlasSprite {
        index: 3,
        anchor: Anchor::TopLeft,
        ..default()
    };
    let sprite_i = TextureAtlasSprite {
        index: 4,
        anchor: Anchor::TopLeft,
        ..default()
    };
    let sprite_j = TextureAtlasSprite {
        index: 5,
        anchor: Anchor::TopLeft,
        ..default()
    };
    let sprite_t = TextureAtlasSprite {
        index: 6,
        anchor: Anchor::TopLeft,
        ..default()
    };
    let sprite_garbage = TextureAtlasSprite {
        index: 9,
        anchor: Anchor::TopLeft,
        ..default()
    };
    let sprite_solid = TextureAtlasSprite {
        index: 8,
        anchor: Anchor::TopLeft,
        ..default()
    };
    let sprite_ghost = TextureAtlasSprite {
        index: 7,
        anchor: Anchor::TopLeft,
        ..default()
    };

    cell_sprites.atlas = texture_atlas_handle.clone();
    cell_sprites.ghost_sprite = sprite_ghost;

    cell_sprites.sprite_map = HashMap::new();
    cell_sprites.sprite_map.insert(CellType::I, sprite_i);
    cell_sprites.sprite_map.insert(CellType::S, sprite_s);
    cell_sprites.sprite_map.insert(CellType::Z, sprite_z);
    cell_sprites.sprite_map.insert(CellType::L, sprite_l);
    cell_sprites.sprite_map.insert(CellType::J, sprite_j);
    cell_sprites.sprite_map.insert(CellType::O, sprite_o);
    cell_sprites.sprite_map.insert(CellType::T, sprite_t);
    cell_sprites.sprite_map.insert(CellType::Garbage, sprite_garbage);
    cell_sprites.sprite_map.insert(CellType::Solid, sprite_solid);


    commands.spawn((SpatialBundle {
        transform: Transform::from_xyz(0.0, 330., 0.0).with_scale(Vec3::new(1., -1., 1.)),
        ..default()
    }, Name::new("board-grid"))).with_children(|commands| {
        for (y, row) in piece_mgr.cell_holder.get_layout().iter().enumerate() {
            for (x, cell) in row.iter().enumerate() {
                // draw grid
                if y < board.game_settings.board.height {
                    commands.spawn(SpriteSheetBundle {
                        texture_atlas: texture_atlas_handle.clone(),
                        sprite: grid_sprite.clone(),
                        transform: Transform::from_xyz(x as f32 * 32.0, y as f32 * 32.0 , 0.0),
                        ..default()
                    });
                }
            }
        }
    });

    commands.spawn((SpatialBundle {
        transform: Transform::from_xyz(0.0, -247., 0.0).with_scale(Vec3::new(1., -1., 1.)),
        ..default()
    }, BoardCellHolderComponent {
        layout: vec![]
    }, Name::new("board-cells"))).with_children(|commands| {
        for (y, row) in piece_mgr.cell_holder.get_layout().iter().rev().enumerate() {
            for (x, cell) in row.iter().enumerate() {

                // draw cells
                if *cell != CellType::None {

                    commands.spawn(BoardCellComponent {
                        cell_type: *cell,
                        position: Point::new(x as i32, y as i32),
                    }).insert(SpriteSheetBundle {
                        texture_atlas: texture_atlas_handle.clone(),
                        sprite: cell_sprites.sprite_map[cell].clone(),
                        transform: Transform::from_xyz(x as f32 * 32.0, y as f32 * 32.0, 1.0),
                        ..default()
                    });

                }
            }
        }
    });

    commands.spawn((TransformBundle {
        local: Transform::from_xyz(128.0, 330., 0.0).with_scale(Vec3::new(1., -1., 1.)),
        ..default()
    }, PieceComponent {})
    ).with_children(|commands| {
        let p = &board.piece_mgr.cur_piece;

        for point in p.get_positions().iter() {
            commands.spawn(SpriteSheetBundle {
                texture_atlas: texture_atlas_handle.clone(),
                sprite: cell_sprites.sprite_map[&p.get_cell_type()].clone(),
                transform: Transform::from_xyz(point.x as f32 * 32.0, point.y as f32 * 32.0, 0.0),
                ..default()
            });
        }
    });


    commands.spawn(board_bundle);

    commands.spawn((SpriteBundle {
        texture: asset_server.load("skins/board_default.png"),
        transform: Transform::from_xyz(171., 38., -1.0),
        sprite: Sprite {
            anchor: Anchor::Center,
            ..default()
        },
        ..default()
    }, Name::new("board-sprite")));
}