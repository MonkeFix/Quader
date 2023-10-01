pub trait Renderable {
    fn render(&self);

    fn debug_render(&self) { }
}