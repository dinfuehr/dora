#![feature(plugin)]

#[plugin] #[no_link]
extern crate phf_mac;
extern crate phf;

use lexer::Lexer;

mod lexer;
mod error;

fn main() {
    let mut reader = Lexer::from_str("fn main {}");

    match reader.read_token() {
        Ok(token) => println!( "token = {:?}", token),
        Err(error) => println!("error = {:?}", error)
    }
}
