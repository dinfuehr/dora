use lexer::reader::{StrReader,CodeReader};

mod lexer;

fn main() {
  let mut reader = StrReader::new("fn main{}");

  loop {
    match reader.read_char() {
      Some(ch) => println!("ch = {}", ch),
      None => break
    }
  }
}
