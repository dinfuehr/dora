use std::env;

pub fn usage() {
    println!("usage: dora [options] file");
}

pub fn parse() -> Result<CmdLine, ()> {
    let mut cmd = CmdLine::new();

    try!(cmd.parse());

    Ok(cmd)
}

pub struct CmdLine {
    filename: Option<String>
}

impl CmdLine {
    pub fn new() -> CmdLine {
        CmdLine {
            filename: None
        }
    }

    pub fn parse(&mut self) -> Result<(), ()> {
        let args : Vec<String> = env::args().collect();

        for arg in &args {
            if let None = self.filename {
                self.filename = Some(arg.clone());
            } else {
                return Err(());
            }
        }

        if let None = self.filename {
            Err(())
        } else {
            Ok(())
        }
    }

    pub fn filename(&self) -> &str {
        self.filename.as_ref().unwrap()
    }
}
