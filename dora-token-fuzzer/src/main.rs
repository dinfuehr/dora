use std::fs;

fn main() {
    let _file = fs::read_to_string(&"test/rt/hello-world.dora").expect("failed to read");
    println!("Hello, world!");
}
