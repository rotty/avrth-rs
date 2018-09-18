macro_rules! primitives {
    ($v:ident, $(fn $fn_name:ident ($vm_id:ident, $name:expr) $body:block)*) => {
        $(fn $fn_name<C: Cell, B: ByteOrder>($vm_id: &mut $crate::forth::vm::Vm<C, B>) -> Result<$crate::forth::vm::Prim<C>, $crate::forth::vm::VmError> { $body Ok($crate::forth::vm::Prim::Continue) })*
        $($v.define_primitive($name, $fn_name));*
    }
}
