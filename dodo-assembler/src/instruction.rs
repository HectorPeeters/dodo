pub type Label = usize;

#[derive(Debug)]
pub enum Instruction<R, C> {
    MovImm(R, C),

    Jmp(Label),
    JmpEq(Label, R, C),
    JmpNe(Label, R, C),

    Add(R, R, R),
    Sub(R, R, R),
    Mul(R, R, R),
    And(R, R, R),
    Or(R, R, R),

    Push(R),
    Pop(R),

    Call(Label, Vec<R>),
    Ret(R),
}
