use std::cell::{Ref, RefCell};
use std::collections::{BTreeMap, HashMap};
use std::fmt;
use std::mem;
use std::rc::Rc;

// TODO: Use custom error type
use anyhow::{anyhow, Error};

struct Block {
    data: Rc<RefCell<Vec<u8>>>,
}

impl Block {
    fn new() -> Self {
        Block {
            data: Rc::new(RefCell::new(vec![])),
        }
    }
    fn len(&self) -> usize {
        self.data.borrow().len()
    }
    fn alloc(&mut self, n_bytes: usize) -> usize {
        let mut data = self.data.borrow_mut();
        let index = data.len();
        data.resize(index + n_bytes, 0);
        index
    }
    fn push(&mut self, n_bytes: usize, value: u32) {
        // TODO: Could be optimized not to call borrow_mut() twice
        let index = self.alloc(n_bytes);
        Self::store(&*self.data, index, n_bytes, value);
    }
    fn push_defer(&mut self, n_bytes: usize) -> impl FnMut(u32) {
        let index = self.alloc(n_bytes);
        let data = self.data.clone();
        move |value| {
            Self::store(&*data, index, n_bytes, value);
        }
    }
    #[allow(dead_code)]
    fn borrow(&self) -> Ref<Vec<u8>> {
        self.data.borrow()
    }
    fn store(data: &RefCell<Vec<u8>>, index: usize, n_bytes: usize, value: u32) {
        let data = &mut data.borrow_mut()[index..];
        let mut v = value;
        for loc in &mut data[..n_bytes] {
            *loc = v as u8;
            v >>= 8;
        }
    }
    // This will panic if there are still outstanding references
    // (i.e., not all returned closures obtained by `push_defer` are
    // dropped).
    fn into_vec(self) -> Vec<u8> {
        Rc::try_unwrap(self.data)
            .expect("outstanding references to block")
            .into_inner()
    }
}

fn mask_(dst: u32, src: u32, mask: u32) -> u32 {
    (dst & !mask) | (src & mask)
}

fn rd_rr_encoder(opcode: u32, mask: u32) -> impl Fn(u32, u32) -> u32 {
    move |rd, rr| {
        let w = mask_(((rr | (rr << 5)) & 0x20F) | (rd << 4), opcode, mask);
        if w & 0xFC07 == 0x9000 {
            (w & 0xEFFF) as u32
        } else {
            w as u32
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ExprType {
    Constant(u8),
    RelativeLabel,
    Register,
    WordRegister,
    RegisterPlus,
    IncrementRegister,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(String), // TODO: maybe use reference
    Int(i32),
    Plus(Vec<Expr>),
    Minus(Vec<Expr>),
    Times(Vec<Expr>),
    Divide(Vec<Expr>),
    Hi8(Box<Expr>),
    Lo8(Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Expr::*;
        match self {
            Ident(ident) => write!(f, "`{}`", ident),
            Int(n) => write!(f, "{}", n),
            _ => unimplemented!(),
        }
    }
}

type DeferredEmit = Box<dyn FnMut(&Assembler) -> Result<(), Error>>;

enum Operand {
    Value(u16),
    Deferred(Box<dyn Fn(&Assembler) -> Result<u16, Error>>),
}

impl Operand {
    fn defer<F>(f: F) -> Self
    where
        F: Fn(&Assembler) -> Result<u16, Error> + 'static,
    {
        Operand::Deferred(Box::new(f))
    }
    fn resolve(&self, assembler: &Assembler) -> Result<u16, Error> {
        match *self {
            Operand::Value(n) => Ok(n),
            Operand::Deferred(ref func) => func(assembler),
        }
    }
}

impl fmt::Debug for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Operand::*;
        match *self {
            Value(n) => write!(f, "Value({})", n),
            Deferred(_) => write!(f, "Deferred(<>)"),
        }
    }
}

impl PartialEq for Operand {
    fn eq(&self, rhs: &Self) -> bool {
        use self::Operand::*;
        match (self, rhs) {
            (Value(n1), Value(n2)) => n1 == n2,
            _ => false,
        }
    }
}

trait CodeEmitter {
    fn emit(
        &self,
        assembler: &Assembler,
        block: &mut Block,
        ip: u16,
        operands: &[Expr],
    ) -> Result<Option<DeferredEmit>, Error>;
}

struct Op2Emitter<E> {
    encoder: Rc<E>,
    types: (ExprType, ExprType),
    size: usize,
}

impl<E> Op2Emitter<E> {
    fn new(encoder: E, types: (ExprType, ExprType), size: usize) -> Self {
        Op2Emitter {
            encoder: Rc::new(encoder),
            types,
            size,
        }
    }
}

impl<E> CodeEmitter for Op2Emitter<E>
where
    E: Fn(u32, u32) -> u32 + 'static,
{
    fn emit(
        &self,
        assembler: &Assembler,
        block: &mut Block,
        _ip: u16,
        operands: &[Expr],
    ) -> Result<Option<DeferredEmit>, Error> {
        if operands.len() != 2 {
            return Err(anyhow!("two arguments expected, got: {:?}", operands));
        }
        match (
            assembler.eval(&operands[0], self.types.0)?,
            assembler.eval(&operands[1], self.types.1)?,
        ) {
            (Operand::Value(v0), Operand::Value(v1)) => {
                block.push(self.size, (self.encoder)(u32::from(v0), u32::from(v1)));
                Ok(None)
            }
            (Operand::Value(v0), Operand::Deferred(fn1)) => {
                let mut emit = block.push_defer(self.size);
                let encoder = self.encoder.clone();
                Ok(Some(Box::new(move |assembler| {
                    emit(encoder(u32::from(v0), u32::from(fn1(assembler)?)));
                    Ok(())
                })))
            }
            (Operand::Deferred(fn0), Operand::Value(v1)) => {
                let mut emit = block.push_defer(self.size);
                let encoder = self.encoder.clone();
                Ok(Some(Box::new(move |assembler| {
                    emit(encoder(u32::from(fn0(assembler)?), u32::from(v1)));
                    Ok(())
                })))
            }
            (Operand::Deferred(fn0), Operand::Deferred(fn1)) => {
                let mut emit = block.push_defer(self.size);
                let encoder = self.encoder.clone();
                Ok(Some(Box::new(move |assembler| {
                    emit(encoder(
                        u32::from(fn0(assembler)?),
                        u32::from(fn1(assembler)?),
                    ));
                    Ok(())
                })))
            }
        }
    }
}

struct InstructionSet(HashMap<String, Box<dyn CodeEmitter>>);

impl InstructionSet {
    fn new() -> Self {
        InstructionSet(HashMap::new())
    }
    fn define(&mut self, name: &str, emitter: impl CodeEmitter + 'static) {
        self.0.insert(name.into(), Box::new(emitter));
    }
    fn get(&self, name: &str) -> Option<&dyn CodeEmitter> {
        self.0.get(name).map(|e| e.as_ref())
    }
}

macro_rules! define_instructions {
    ($inst:expr, ($emitter:ident, $encoder:expr, $types:expr, $size:expr) { $($name:ident ($($args:expr),*)),* $(,)*}) => {
        $({ let specific_encoder = $encoder($($args),*);
            $inst.define(stringify!($name), $emitter::new(specific_encoder, $types, $size));
        })*
    }
}

pub struct Assembler {
    registers: HashMap<String, u8>,
    instructions: InstructionSet,
    symbols: HashMap<String, i32>,
    pc: u16,
    blocks: BTreeMap<u16, Block>,
    defers: Vec<DeferredEmit>,
    current_block: Option<(u16, Block)>,
}

fn avr_instruction_set() -> InstructionSet {
    use self::ExprType::*;

    let mut inst = InstructionSet::new();
    define_instructions!(inst, (Op2Emitter, rd_rr_encoder, (Register, Register), 2) {
        mul    (0x9C00, 0xFC00),
        muls   (0x0200, 0xFF00),
        mulsu  (0x0300, 0xFF88),
        fmul   (0x0308, 0xFF88),
        fmuls  (0x0380, 0xFF88),
        fmulsu (0x0388, 0xFF88),
        cpc    (0x0400, 0xFC00),
        sbc    (0x0800, 0xFC00),
        add    (0x0C00, 0xFC00),
        cpse   (0x1000, 0xFC00),
        cp     (0x1400, 0xFC00),
        sub    (0x1800, 0xFC00),
        adc    (0x1C00, 0xFC00),
        and    (0x2000, 0xFC00),
        eor    (0x2400, 0xFC00),
        or     (0x2800, 0xFC00),
        mov    (0x2C00, 0xFC00),   // 2 3 mov,  R2<--R3
    });
    define_instructions!(inst, (Op2Emitter, rd_rr_encoder, (Register, RegisterPlus), 2) {
        ld     (0x9000, 0xFE00), //  ( Rd Rr -- ), \ Rr={Z+,-Z,Y+,-Y,X+,-X,X,Y,Z}
        lpm_   (0x9004, 0xFE0E), //  ( Rd Rr -- ), \ Rr={Z,Z+}, 2 Z+ lpm_
        elpm_  (0x9006, 0xFE0E), //  ( Rd Rr -- ), \ Rr={Z,Z+}
    });
    inst
}

fn avr_registers() -> HashMap<String, u8> {
    let mut registers = HashMap::new();
    for i in 0..32 {
        registers.insert(format!("r{}", i), i);
    }
    for (name, base) in &[("x", 26), ("y", 28), ("z", 30)] {
        for (suffix, i) in &[("l", 0), ("h", 1)] {
            registers.insert(format!("{}{}", name, suffix), (base + i) as u8);
        }
    }
    registers
}

impl Default for Assembler {
    fn default() -> Self {
        Self::new()
    }
}

impl Assembler {
    pub fn new() -> Self {
        Assembler {
            registers: avr_registers(),
            instructions: avr_instruction_set(),
            symbols: HashMap::new(),
            pc: 0,
            blocks: BTreeMap::new(),
            defers: vec![],
            current_block: None,
        }
    }
    pub fn assemble(&mut self, mnemonic: &str, args: &[Expr]) -> Result<(), Error> {
        let pc = self.pc();
        let mut current = self.current_block.take();
        let (block_base, block) = current.get_or_insert_with(|| (pc, Block::new()));
        let emitter = self
            .instructions
            .get(mnemonic)
            .ok_or_else(|| anyhow!("unknown mnemonic `{}`", mnemonic))?;
        // FIXME: check pc fits into u16
        let pc = *block_base + block.len() as u16;
        if let Some(defer) = emitter.emit(self, block, pc, args)? {
            self.defers.push(defer);
        }
        self.current_block = current;
        Ok(())
    }
    pub fn assemble_iter<'a, I>(&mut self, instructions: I) -> Result<(), Error>
    where
        I: IntoIterator<Item = &'a (&'a str, &'a [Expr])>,
    {
        for (mnemonic, args) in instructions.into_iter() {
            self.assemble(mnemonic, args)?;
        }
        Ok(())
    }
    pub fn flush(&mut self) -> Result<BTreeMap<u16, Vec<u8>>, Error> {
        if let Some((block_base, block)) = self.current_block.take() {
            self.pc = block_base + block.len() as u16;
            self.blocks.insert(block_base, block);
        }
        // Empty our state first, so it is defined in case of failure.
        let blocks = mem::take(&mut self.blocks);
        let defers = mem::take(&mut self.defers);
        for mut emit in defers.into_iter() {
            emit(self)?;
        }
        Ok(blocks
            .into_iter()
            .map(|(address, block)| (address, block.into_vec()))
            .collect())
    }
    fn eval(&self, expr: &Expr, expr_type: ExprType) -> Result<Operand, Error> {
        use self::ExprType::*;
        match expr_type {
            Constant(n_bits) => self.eval_int(expr, n_bits),
            Register => match expr {
                Expr::Ident(ref name) => self
                    .registers
                    .get(name)
                    .map(|i| Operand::Value(u16::from(*i)))
                    .ok_or_else(|| anyhow!("invalid register name `{}`", name)),
                _ => Err(anyhow!("expected register name, found `{}`", expr)),
            },
            _ => unimplemented!(),
        }
    }
    fn eval_int(&self, expr: &Expr, n_bits: u8) -> Result<Operand, Error> {
        fn check_range(expr: &Expr, n: i32, n_bits: u8) -> Result<u16, Error> {
            let max_k = 1 << n_bits;
            let min_k = -(max_k >> 1);
            if min_k <= n && n < max_k {
                Ok(n as u16)
            } else {
                Err(anyhow!(
                    "value out of range: {} (evaluating to {}) is not within [{}..{})",
                    expr,
                    n,
                    min_k,
                    max_k
                ))
            }
        }
        match *expr {
            Expr::Int(n) => Ok(Operand::Value(check_range(expr, n, n_bits)?)),
            Expr::Ident(ref name) => match self.symbol_lookup(&name) {
                Some(n) => Ok(Operand::Value(check_range(expr, n, n_bits)?)),
                None => {
                    let name = name.clone();
                    let expr = expr.clone();
                    Ok(Operand::Deferred(Box::new(move |assembler| {
                        let value = assembler
                            .symbol_lookup(&name)
                            .ok_or_else(|| anyhow!("undefined symbol {}", name))?;
                        check_range(&expr, value, n_bits)
                    })))
                }
            },
            Expr::Plus(ref exprs) => self.eval_arithmetic_op(&exprs, |assembler, operands| {
                operands.iter().map(|op| op.resolve(assembler)).sum()
            }),
            Expr::Minus(ref exprs) => self.eval_arithmetic_op(&exprs, |assembler, operands| {
                let mut iter = operands.iter();
                let mut result = iter
                    .next()
                    .ok_or_else(|| anyhow!("too few arguments for operator `-`"))?
                    .resolve(assembler)?;
                for op in iter {
                    result -= op.resolve(assembler)?;
                }
                Ok(result)
            }),
            _ => unimplemented!(),
        }
    }
    fn eval_arithmetic_op<F>(&self, exprs: &[Expr], func: F) -> Result<Operand, Error>
    where
        F: Fn(&Assembler, &[Operand]) -> Result<u16, Error> + 'static,
    {
        let operands = exprs
            .iter()
            .map(|e| self.eval_int(e, 30))
            .collect::<Result<Vec<_>, _>>()?;
        let defer = operands.iter().any(|op| match op {
            Operand::Deferred(_) => true,
            Operand::Value(_) => false,
        });
        if defer {
            Ok(Operand::defer(move |assembler| func(assembler, &operands)))
        } else {
            Ok(Operand::Value(func(self, &operands)?))
        }
    }
    pub fn pc(&self) -> u16 {
        self.pc
    }
    fn symbol_lookup(&self, name: &str) -> Option<i32> {
        self.symbols.get(name).copied()
    }
    pub fn define_symbol(&mut self, name: &str, value: i32) {
        self.symbols.insert(name.into(), value);
    }
}

#[cfg(test)]
mod tests {
    use super::ExprType::*;
    use super::Operand::*;
    use super::*;

    #[test]
    fn test_eval_int() {
        let assembler = Assembler::new();
        assert_eq!(
            assembler.eval(&Expr::Int(42), Constant(8)).unwrap(),
            Value(42)
        );
    }

    #[test]
    fn test_eval_int_erange() {
        let assembler = Assembler::new();
        assert!(assembler.eval(&Expr::Int(257), Constant(8)).is_err());
    }

    #[test]
    fn test_eval_plus() {
        let assembler = Assembler::new();
        assert_eq!(
            assembler
                .eval(
                    &Expr::Plus(vec![Expr::Int(23), Expr::Int(9), Expr::Int(10)]),
                    Constant(8)
                )
                .unwrap(),
            Value(42)
        );
    }

    fn op2_encoder(n1: u32, n2: u32) -> u32 {
        (n1 & 0xFF) << 8 | n2 & 0xFF
    }

    #[test]
    fn test_op2_emitter() {
        let assembler = Assembler::new();
        let emitter = Op2Emitter::new(op2_encoder, (Constant(8), Constant(8)), 2);
        let mut block = Block::new();
        emitter
            .emit(
                &assembler,
                &mut block,
                0,
                &[Expr::Int(0x42), Expr::Int(0x67)],
            )
            .unwrap();
        assert_eq!(&block.borrow()[..], &[0x67, 0x42]);
    }

    #[test]
    fn test_inst_mul() {
        let mut assembler = Assembler::new();
        assembler
            .assemble(
                "mul",
                vec![Expr::Ident("r16".into()), Expr::Ident("r10".into())].as_slice(),
            )
            .unwrap();
        let blocks: Vec<_> = assembler
            .flush()
            .expect("could not flush")
            .into_iter()
            .collect();
        assert_eq!(blocks, vec![(0, vec![0x0a, 0x9d])])
    }
}
