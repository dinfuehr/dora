use std::io::fs::File;
use std::io::{BufferedReader,IoError};
use std::result::Result;
use std::fmt::{Formatter,Show,Error};

use lexer::position::Position;
use lexer::token::{Token,TokenType};
use lexer::char::Character;
use error::ParseError;

pub mod token;
pub mod position;
pub mod char;

trait FileReader {
    fn read_char(&mut self);
}

pub struct Lexer {
    filename: String,
    file: BufferedReader<Result<File,IoError>>,
    looks: Vec<Character>
}

impl Lexer {
    pub fn new( filename : &str ) -> Lexer {
        let file = File::open(&Path::new(filename));
        let mut lex = Lexer {
            filename: filename.to_string(),
            file: BufferedReader::new(file),
            looks: vec![]
        };

        lex
    }
}
