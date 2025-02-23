#![allow(dead_code)]

use std::path::PathBuf;

use clap::Parser;
use either::Either::{self, Left, Right};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use serdebug::SerDebug;
use thiserror::Error;
use vec1::{Size0Error, Vec1};

#[derive(Serialize, Deserialize, SerDebug)]
struct Program {
    #[serde(skip_serializing_if = "Vec::is_empty")]
    elements: Vec<Element>,
}

#[derive(Serialize, Deserialize, SerDebug)]
struct Function {
    #[serde(skip_serializing_if = "IndexMap::is_empty")]
    args: IndexMap<String, Typ>,
}

#[derive(Serialize, Deserialize, SerDebug)]
struct Struct {
    #[serde(skip_serializing_if = "IndexMap::is_empty")]
    properties: IndexMap<String, Typ>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    generics: Vec<Typ>,
}

fn vec1_is_empty<T>(v: &Vec1<T>) -> bool {
    v.len() == 0
}

#[derive(Serialize, Deserialize, SerDebug)]
struct Actor {
    #[serde(skip_serializing_if = "IndexMap::is_empty")]
    properties: IndexMap<String, Typ>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    generics: Vec<Typ>,
    #[serde(skip_serializing_if = "vec1_is_empty")]
    handlers: Vec1<Handler>,
}

#[derive(Serialize, Deserialize, SerDebug)]
struct Atom(String);

#[derive(Serialize, Deserialize, SerDebug)]
enum TypOrAtom {
    Typ(Typ),
    Atom(Atom),
}

#[derive(Serialize, Deserialize, SerDebug)]
struct Handler {
    pattern: Vec1<TypOrAtom>,
}

#[derive(Serialize, Deserialize, SerDebug)]
struct Typ {
    parts: Vec1<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    generics: Vec<Typ>,
}

#[derive(Debug, Error)]
enum ParseError {
    #[error("Function \"{name}\" already defined.")]
    FunctionAlreadyDefined { name: String },
    #[error("Property \"{0}\" already defined.")]
    DuplicateActorProperty(String),
    #[error("Type needs to contain at least one identifier.")]
    NoIdentInType(#[from] Size0Error),
}

type ParseResult<T> = Result<T, ParseError>;

#[derive(Serialize, Deserialize, SerDebug)]
enum Element {
    Import(PathBuf),
    Atoms(Vec<String>),
    Func(Named<Function>),
    Struct(Named<Struct>),
    Actor(Named<Actor>),
    Comment(String),
}

#[derive(Serialize, Deserialize, SerDebug)]
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
      / _ a:atoms() _ { Ok(Element::Atoms(a)) }
      / _ f:func() _ { Ok(Element::Func(f?)) }
      / _ s:r#struct() _ { Ok(Element::Struct(s?)) }
      / _ a:actor() _ { Ok(Element::Actor(a?)) }

    rule import() -> PathBuf
      = "import" _ s:string() _ ";" { PathBuf::from(s) }

    rule atoms() -> Vec<String>
      = "atom" _ s:(atom() ** ",") _ ";" { s }

    rule atom() -> String
      = _ a:("@" i:(ident() / string()) {i}) _ { a }
    / expected!("atom")

    rule func() -> ParseResult<Named<Function>>
      = "fn" _ name:ident() _ "(" _ args:(arg:arg() ** "," { arg }) _ ")" _ "{" _ "}" {
          Ok(Named::new(name, Function {
          args: args.into_iter().collect::<ParseResult<Vec<_>>>()?.into_iter().map(Named::to_tuple).collect()
        }))
      }
    / expected!("func")

    rule arg() -> ParseResult<Named<Typ>>
      = _ name:ident() _ ":" _ t:typ() _ { Ok(Named::new(name, t?)) }
      / expected!("arg")

    rule r#struct() -> ParseResult<Named<Struct>>
      = _ "struct" _ name:ident() _ g:("<" _ gs:(typ() ** ",")  _ ">" { gs })? _ "{" _ ps:(property() ** ",") _ "}" {
        Ok(Named::new(name,
          Struct {
            properties: ps.into_iter().collect::<ParseResult<Vec<_>>>()?.into_iter().map(Named::to_tuple).collect(),
            generics: g.unwrap_or_default().into_iter().collect::<ParseResult<Vec<Typ>>>()?
          }
        ))
      }
      / expected!("struct")

    rule actor() -> ParseResult<Named<Actor>>
      = _ "actor" _ name:ident() _ g:("<" _ gs:(typ() ** ",")  _ ">" { gs })? _ "{" _ ps:(property_or_handler() ** ",") _ "}" {
        let mut handlers = Vec::new();
        let mut properties = IndexMap::new();
        for poh in ps.into_iter().collect::<ParseResult<Vec<_>>>()? {
          match poh {
            Left(p) => if properties.insert(p.name.clone(), p.value).is_some() {
              Err(ParseError::DuplicateActorProperty(p.name))?;
            },
            Right(h) => handlers.push(h),
          }
        }
        Ok(Named::new(name,
          Actor {
          properties,
            generics: g.unwrap_or_default().into_iter().collect::<ParseResult<Vec<Typ>>>()?,
            handlers: Vec1::try_from_vec(handlers)?
          }
        ))
      }
      / expected!("actor")

    rule property() -> ParseResult<Named<Typ>>
      = _ name:ident() _ ":" _ t:typ() _ { Ok(Named::new(name, t?)) }
      / expected!("property")

    rule handler() -> ParseResult<Handler>
      = _ "(" _ aot:(typ_or_atom() ** ",") _ ")" _ "->" _ "{" _ "}" _ {
        Ok(Handler{
          pattern: Vec1::try_from_vec(aot.into_iter().collect::<ParseResult<Vec<_>>>()?)?
          })
        }
      / expected!("handler")

    rule property_or_handler() -> ParseResult<Either<Named<Typ>, Handler>>
      = _ p:property() _ { Ok(Left(p?)) }
      / _ h:handler() _ { Ok(Right(h?)) }
      / expected!("property or handler")

    rule typ_or_atom() -> ParseResult<TypOrAtom>
      = _ a:atom() _ { Ok(TypOrAtom::Atom(Atom(a)))}
      / _ t:typ() _ { Ok(TypOrAtom::Typ(t?))}
      / expected!("typ or atom")

    rule typ() -> ParseResult<Typ>
      = p:(ident() ** "::") g:(_ "<" _ gs:(typ() ** ",")  _ ">" { gs })? {
        Ok(Typ {
          parts: Vec1::try_from_vec(p)?,
          generics: g.unwrap_or_default().into_iter().collect::<ParseResult<Vec<Typ>>>()?
        })
      }
      / expected!("typ")

    rule ident() -> String
      = quiet!{i:$([ 'a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_' ]*)  { i.to_string() }}
      / expected!("identifier")

    rule string() -> String
      = "\"" s:$(([^'"'] / "\\\"")*) "\"" { s.to_string() }
      / expected!("string")

    rule _()
      = ("\r\n" / [' ' |  '\n' | '\t'])*

    rule __()
      = ([' ' |'\t'])*

    rule ___()
      = ("\r\n" / "\n")+
  }
}

#[derive(Parser)]
struct Args {
    path: PathBuf,
}

pub fn main() -> anyhow::Result<()> {
    let Args { path } = Args::parse();
    let src = std::fs::read_to_string(path)?;
    let parsed = list_parser::program(&src)??;
    println!("{}", serde_yaml::to_string(&parsed)?);
    Ok(())
}
