use lexer::reader::StrReader;
use lexer::Lexer;

mod lexer;
mod error;

fn main() {
    let reader = Lexer::new(StrReader::new("fn main {}"));

    match reader.read_token() {
        Some(token) => println!( "token = {}", token),
        None => println!("no token")
    }
}
