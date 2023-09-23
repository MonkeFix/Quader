extern crate sdl2;

use std::time::Duration;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;

fn main() -> Result<(), String> {
    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;

    let windows = video_subsystem
        .window("Quader Engine Samples", 800, 600)
        .position_centered()
        .vulkan()
        .build()
        .map_err(|e| e.to_string())?;

    let mut canvas = windows.into_canvas().build().map_err(|e| e.to_string())?;

    canvas.set_draw_color(Color::RGB(255, 0, 0));
    canvas.clear();
    canvas.present();

    let mut event_pump = sdl_context.event_pump()?;

    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running,
                _ => {}
            }
        }

        canvas.clear();
        canvas.present();
        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 30));
        // The rest of the game loop goes here...
    }

    Ok(())
}
