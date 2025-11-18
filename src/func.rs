use crate::lexer::Type;
use chumsky::span::SimpleSpan;

pub struct FunctionMapper(pub Box<dyn Fn(Vec<Type>) -> Type>);

impl FunctionMapper {
    pub fn from(value: Box<dyn Fn(Vec<Type>) -> Type>) -> Self {
        Self(value)
    }
}

pub struct FunctionMap {
    map: FunctionMapper,
    pub params: Vec<Vec<Type>>,
    /// starting on [`usize::MAX`] represents a builtin
    span: SimpleSpan,
    name: String,
}

impl FunctionMap {
    fn new<F: Fn(Vec<Type>) -> Type + 'static>(name: &'static str, fnn: F) -> Self {
        Self {
            map: FunctionMapper(Box::new(fnn)),
            params: vec![],
            span: SimpleSpan::from(usize::MAX..usize::MAX),
            name: name.to_string(),
        }
    }
    pub fn newm(name: String, mapper: FunctionMapper, span: SimpleSpan) -> Self {
        Self {
            map: mapper,
            params: vec![],
            span,
            name,
        }
    }

    pub(crate) fn mapper(&self) -> &Box<dyn Fn(Vec<Type>) -> Type> {
        &self.map.0
    }

    pub(crate) fn name(&self) -> String {
        self.name.to_string()
    }

    pub(crate) fn params(&self) -> &Vec<Vec<Type>> {
        &self.params
    }
}

// Number*1 -> Number*1 with broadcasting mapping function
fn n1n1b(pars: Vec<Type>) -> Type {
    let t = pars[0].clone();
    if let Type::List(_) = t {
        t
    } else if t == Type::Num {
        t
    } else {
        unimplemented!("Normal types for now please :-)")
    }
}

pub fn builtin_funcs() -> [FunctionMap; 1] {
    use crate::func::FunctionMap as FM;
    [FM::new("sin", n1n1b)]
}
