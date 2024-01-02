use macroquad::prelude::*;
use crate::board_controller::BoardController;
use crate::board_manager::BoardManager;

pub struct GameRoot {
    close_requested: bool,
    board_manager: Box<BoardManager>,
    debug_render_enabled: bool
}

impl GameRoot {
    pub fn new() -> Self {

        GameRoot {
            close_requested: false,
            board_manager: Box::new(BoardManager::new()),
            debug_render_enabled: true
        }
    }

    pub async fn run(&mut self) {

        self.load_content().await;

        'main_loop: loop {
            if is_key_pressed(KeyCode::Escape) {
                self.close();
            }

            if self.close_requested {
                break 'main_loop;
            }

            self.update(get_frame_time());
    
            clear_background(BLACK);
            self.render();
            self.render_ui();
    
            macroquad_profiler::profiler(Default::default());

            next_frame().await
        }
    }

    async fn load_content(&mut self) {
        self.board_manager.load_content().await;
    }

    pub fn close(&mut self) {
        self.close_requested = true;
    }
    
    fn update(&mut self, dt: f32) {
        self.board_manager.update(dt);
    }

    fn render(&mut self) {
        self.board_manager.render();

        /*if self.debug_render_enabled {
            self.board.debug_render();
        }*/
    }

    fn render_ui(&self) {
        /*let (fps, frame_time, time) = (macroquad::time::get_fps(), macroquad::time::get_frame_time(), macroquad::time::get_time());

        root_ui().window(hash!(), Vec2::new(800., 20.), Vec2::new(450., 200.), |ui| {
            let (mouse_x, mouse_y) = mouse_position();
            ui.label(None, &format!("Mouse position: {} {}", mouse_x, mouse_y));

            ui.label(None, &format!("FPS: {}", fps));
            ui.label(None, &format!("Frame Time: {:.6}", frame_time));
            ui.label(None, &format!("Total Time: {:.3}", time));

            let (mouse_wheel_x, mouse_wheel_y) = mouse_wheel();
            ui.label(None, &format!("Mouse wheel x: {}", mouse_wheel_x));
            ui.label(None, &format!("Mouse wheel y: {}", mouse_wheel_y));

            widgets::Group::new(hash!(), Vec2::new(200., 90.))
                .position(Vec2::new(240., 0.))
                .ui(ui, |ui| {
                    ui.label(None, "Pressed kbd keys");

                    if let Some(key) = get_last_key_pressed() {
                        ui.label(None, &format!("{:?}", key))
                    }
                });

            widgets::Group::new(hash!(), Vec2::new(200., 90.))
                .position(Vec2::new(240., 92.))
                .ui(ui, |ui| {
                    ui.label(None, "Pressed mouse keys");

                    if is_mouse_button_down(MouseButton::Left) {
                        ui.label(None, "Left");
                    }
                    if is_mouse_button_down(MouseButton::Right) {
                        ui.label(None, "Right");
                    }
                    if is_mouse_button_down(MouseButton::Middle) {
                        ui.label(None, "Middle");
                    }
                });
        });*/
    }
}