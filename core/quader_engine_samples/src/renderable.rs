pub trait Renderable {
    fn render(&self);

    fn debug_render(&mut self) { }
}