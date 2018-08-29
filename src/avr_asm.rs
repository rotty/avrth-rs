use std::collections::HashMap;

use asm::Assembler;

pub struct AvrAsm {
    symbols: HashMap<String, u16>,
}

pub struct Options {}

impl AvrAsm {
    pub fn new(_options: Options) -> Self {
        let mut symbols = HashMap::new();
        symbols.insert("DO_COLON".into(), 0xBEEF); // FIXME: remove mock
        AvrAsm { symbols: symbols }
    }
}

impl Assembler<u16> for AvrAsm {
    fn define(&mut self, name: &str, value: u16) {
        self.symbols.insert(name.into(), value);
    }
    fn symbol(&self, name: &str) -> Option<u16> {
        self.symbols.get(name).cloned()
    }
}
