use lexer::reader::{StrReader,FileReader};

mod lexer;

fn main() {
  //let mut reader = StrReader::new("fn main{}");
  let mut reader = FileReader::new("fn main{}");

  loop {
    match reader.read_char() {
      Some(ch) => println!("ch = {}", ch),
      None => break
    }
  }
}
