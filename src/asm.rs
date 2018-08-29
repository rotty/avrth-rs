/// Assembler with code addresses type `A`
pub trait Assembler<A> {
    fn define(&mut self, name: &str, value: A);
    fn symbol(&self, name: &str) -> Option<A>;
}
