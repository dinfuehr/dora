use lexer::reader::CodeReader;
use lexer::token::Token;

pub mod reader;
pub mod token;
pub mod position;

pub struct Lexer<T : CodeReader> {
    reader : T
}

impl<T : CodeReader> Lexer<T> {
    pub fn new(reader : T) -> Lexer<T> {
        Lexer::<T> { reader: reader }
    }

    pub fn read_token(&self) -> Option<Token> {
        None
    }
}

//#[cfg(test)]
//mod tests {
    //use super::*;

    //#[test]
    //fn read_numbers() {
        //let reader = Lexer::new(StrReader::new("1 2 345 012"));
    //}
//}

