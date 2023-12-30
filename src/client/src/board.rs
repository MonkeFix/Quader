use std::sync::Arc;
use bevy::input::keyboard::KeyboardInput;
use bevy::prelude::*;
use bevy::sprite::Anchor;
use quader_engine::board::Board;
use quader_engine::cell_holder::CellHolder;
use quader_engine::game_settings::GameSettings;
use quader_engine::piece::{Piece, RotationDirection};
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

#[derive(Bundle, Debug)]
pub struct BoardBundle {
    position: Position,
    board: BoardComponent
}

impl Default for BoardBundle {
    fn default() -> Self {
        Self {
            position: Position { val: Vec2::new(64., 64.) },
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
    mut commands: Commands,
    keyboard_input: Res<Input<KeyCode>>,
    mut q: Query<(Entity, &mut BoardComponent), With<BoardComponent>>
) {

    if q.is_empty() {
        return;
    }

    let mut board_ent = q.single_mut();
    let entity = board_ent.0;
    let mut board = &mut board_ent.1.board;

    if keyboard_input.just_pressed(KeyCode::Z) {
        board.rotate(RotationDirection::CounterClockwise);
    }

    if keyboard_input.just_pressed(KeyCode::X) {
        board.rotate(RotationDirection::Clockwise);
    }

    if keyboard_input.just_pressed(KeyCode::Space) {
        for entity in q.iter() {
            commands.entity(entity.0).despawn();
        }
    }
}

fn test(mut command: Commands) {
    let aa = TextureAtlasSprite::new(1);
}

fn build_system(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut texture_atlases: ResMut<Assets<TextureAtlas>>
) {

    let board_bundle = BoardBundle::default();
    let board = &board_bundle.board.board;
    let piece = &board.get_piece_mgr().curr_piece;

    let texture_handle = asset_server.load("skins/default_3.png");

    let atlas = TextureAtlas::from_grid(
        texture_handle,
        Vec2::new(32., 32.),
        12,
        1,
        None,
        None
    );
    let texture_atlas_handle = texture_atlases.add(atlas);
    let mut sprite = TextureAtlasSprite::new(0);
    sprite.anchor = Anchor::TopLeft;

    for p in piece.get_positions() {
        commands.spawn(SpriteSheetBundle {
            texture_atlas: texture_atlas_handle.clone(),
            sprite: sprite.clone(),
            transform: Transform::from_xyz(32.0 * p.x as f32, 32.0 * p.y as f32, 0.),
            ..default()
        });
    }

    commands.spawn(board_bundle);



    //commands.spawn(BoardA::new())

    

    // commands.spawn(SpriteBundle {
    //     texture: asset_server.load("skins/board_default.png"),
    //     transform: Transform::from_xyz(0., 0., 0.),
    //     sprite: Sprite {
    //         anchor: Anchor::TopLeft,
    //         ..default()
    //     },
    //     ..default()
    // });

    

    // commands.spawn(SpriteSheetBundle {
    //     texture_atlas: texture_atlas_handle,
    //     sprite,
    //     transform: Transform::from_xyz(-200., 0., 0.),
    //     ..default()
    // });
}