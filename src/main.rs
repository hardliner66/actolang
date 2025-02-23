#![allow(dead_code)]

use std::path::PathBuf;

use indexmap::IndexMap;
use thiserror::Error;
use vec1::{Size0Error, Vec1};

#[derive(Debug)]
struct Program {
    elements: Vec<Element>,
}

#[derive(Debug)]
struct Function {
    args: IndexMap<String, Typ>,
}

#[derive(Debug)]
struct Struct {
    properties: IndexMap<String, Typ>,
    generics: Vec<Typ>,
}

#[derive(Debug)]
struct Typ {
    parts: Vec1<String>,
    generics: Vec<Typ>,
}

#[derive(Debug, Error)]
enum ParseError {
    #[error("Function \"{name}\" already defined.")]
    FunctionAlreadyDefined { name: String },
    #[error("Type needs to contain at least one identifier.")]
    NoIdentInType(#[from] Size0Error),
}

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
enum Element {
    Import(PathBuf),
    Func(Named<Function>),
    Struct(Named<Struct>),
    Comment(String),
}

#[derive(Debug)]
struct Named<T> {
    name: String,
    value: T,
}

impl<T> Named<T> {
    fn new(name: String, value: T) -> Self {
        Self { name, value }
    }

    fn to_tuple(self) -> (String, T) {
        (self.name, self.value)
    }
}

peg::parser! {
  grammar list_parser() for str {
    pub rule program() -> ParseResult<Program>
      = _ elements:element()* {
        Ok(Program { elements: elements.into_iter().collect::<ParseResult<Vec<_>>>()? })
    }

    rule element() -> ParseResult<Element>
      = s:$(_ "//" [^'\n']* __) ___ { Ok(Element::Comment(s.to_string())) }
      / _ i:import() _ { Ok(Element::Import(i)) }
      / _ f:func() _ { Ok(Element::Func(f?)) }
      / _ s:r#struct() _ { Ok(Element::Struct(s?)) }

    rule import() -> PathBuf
      = "import" _ s:string() _ ";" { PathBuf::from(s) }

    rule func() -> ParseResult<Named<Function>>
      = "fn" _ name:ident() _ "(" _ args:(arg:arg() ** "," { arg }) _ ")" _ "{" _ "}" {
        Ok(Named::new(name, Function {
        args: args.into_iter().collect::<ParseResult<Vec<_>>>()?.into_iter().map(Named::to_tuple).collect()
      }))
    }

    rule arg() -> ParseResult<Named<Typ>>
      = _ name:ident() _ ":" _ t:typ() _ { Ok(Named::new(name, t?)) }

    rule r#struct() -> ParseResult<Named<Struct>>
      = _ "struct" _ name:ident() _ g:("<" _ gs:(typ() ** ",")  _ ">" { gs })? _ "{" _ ps:(property() ** ",") _ "}" {
        Ok(Named::new(name,
          Struct {
            properties: ps.into_iter().collect::<ParseResult<Vec<_>>>()?.into_iter().map(Named::to_tuple).collect(),
            generics: g.unwrap_or_default().into_iter().collect::<ParseResult<Vec<Typ>>>()?
          }
        ))
      }

    rule property() -> ParseResult<Named<Typ>>
      = _ name:ident() _ ":" _ t:typ() _ { Ok(Named::new(name, t?)) }

    rule typ() -> ParseResult<Typ>
      = p:(ident() ** "::") g:(_ "<" _ gs:(typ() ** ",")  _ ">" { gs })? {
        Ok(Typ {
          parts: Vec1::try_from_vec(p)?,
          generics: g.unwrap_or_default().into_iter().collect::<ParseResult<Vec<Typ>>>()?
        })
      }

    rule ident() -> String
      = quiet!{i:$([ 'a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_' ]*)  { i.to_string() }}
      / expected!("identifier")

    rule string() -> String
      = "\"" s:$(([^'"'] / "\\\"")*) "\"" { s.to_string() }

    rule _()
      = ("\r\n" / [' ' |  '\n' | '\t'])*

    rule __()
      = ([' ' |'\t'])*

    rule ___()
      = ("\r\n" / "\n")+
  }
}

const SRC: &str = r#"
import "some/path/to/file.act";

// my cool comment

struct SomeMessage <T> {
  a: i32,
  b: String
}

fn main() {

}

fn add(a: i32, b: std::vec::Vec<i32>) {}
"#;

pub fn main() -> anyhow::Result<()> {
    println!("{:#?}", list_parser::program(SRC)??);
    Ok(())
}
