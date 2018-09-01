pub mod shim;

/// Target with code addresses of type `A`
pub trait Target<A> {
    /// Emit startup code
    ///
    /// This should emit startup code for the target, including the
    /// inner interpreter at address `address`, and return the next
    /// free address.
    fn emit_startup_code(&mut self, address: A) -> A;

    fn define(&mut self, name: &str, value: A);

    fn symbol(&self, name: &str) -> Option<A>;
}
