use lexer::reader::StrReader;
use lexer::Lexer;

mod lexer;
mod error;

fn main() {
    let mut reader = Lexer::new(StrReader::new("fn main {}"));

    match reader.read_token() {
        Ok(token) => println!( "token = {:?}", token),
        Err(error) => println!("error = {:?}", error)
    }
}
