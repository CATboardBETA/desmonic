use crate::lexer::Type;

// TODO: Try making these arrays with const generics
#[derive(Clone)]
pub struct FunctionMap {
    input: Vec<Type>,
    output: Type,
    broadcast: Vec<bool>,
}

impl FunctionMap {
    pub fn new<A: AsRef<[(Type, bool)]>>(inputs: A, output: Type) -> Self {
        let (input, broadcast): (Vec<_>, Vec<_>) = inputs.as_ref().into_iter().cloned().unzip();
        Self {
            input,
            output,
            broadcast,
        }
    }
    pub fn new_user(input: Vec<Type>, output: Type) -> Self {
        let mut broadcast = vec![];
        for i in input.iter() {
            broadcast.push(if *i == Type::Num { true } else { false })
        }
        Self {
            input,
            output,
            broadcast,
        }
    }

    pub fn broadcast(&self, input: Vec<Type>) -> Type {
        if input.len() != self.input.len() {
            unreachable!()
        }
        let mut out = vec![];
        for ((i, ri), b) in self
            .input
            .iter()
            .cloned()
            .zip(input)
            .zip(self.broadcast.iter().cloned())
        {
            if b && matches!(ri, Type::List(t) if *t == i || Type::List(t.clone()) == i ) {
                out.push(if !matches!(self.output, Type::List(_)) {
                    Type::List(Box::new(self.output.clone()))
                } else {
                    self.output.clone()
                })
            } else {
                out.push(self.output.clone())
            }
        }
        if let Some(idx) = out
            .iter()
            .enumerate()
            .filter(|&(_, b)| matches!(b, Type::List(_)))
            .map(|(idx, _)| idx)
            .nth(0)
        {
            out[idx].clone()
        } else {
            out[0].clone()
        }
    }
}

pub fn builtin_funcs() -> Vec<(&'static str, FunctionMap)> {
    use FunctionMap as FM;
    use Type::*;
    vec![("sin", FM::new([(Num, true)], Num))]
}
