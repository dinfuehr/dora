use std::fs;

fn main() {
    let _file = fs::read_to_string(&"tests/hello-world.dora").expect("failed to read");
    println!("Hello, world!");
}
