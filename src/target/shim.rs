use std::collections::HashMap;

use super::Target;
use forth::vm::Cell;

pub struct ShimTarget<C: Cell> {
    symbols: HashMap<String, C>,
}

impl<C: Cell> ShimTarget<C> {
    pub fn new() -> Self {
        ShimTarget {
            symbols: HashMap::new(),
        }
    }
}

impl<C: Cell + 'static> Target<C> for ShimTarget<C> {
    fn emit_startup_code(&mut self, address: C) -> C {
        self.define("DO_COLON", address);
        address + C::one()
    }
    fn define(&mut self, name: &str, value: C) {
        self.symbols.insert(name.into(), value);
    }
    fn symbol(&self, name: &str) -> Option<C> {
        self.symbols.get(name).cloned()
    }
}
