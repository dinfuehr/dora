use lexer::Lexer;

mod error;
mod lexer;

fn main() {
    let lex = Lexer::new("test/hello.dora");
    println!("hello");
}
