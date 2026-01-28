mod driver;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    driver::start()
}
