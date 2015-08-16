use std::env;

pub fn usage() {
    println!("usage: dora [options] file");
}

pub fn parse() -> Result<CmdLine, String> {
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

    fn parse(&mut self) -> Result<(), String> {
        let args : Vec<String> = env::args().skip(1).collect();

        for arg in &args {
            if let None = self.filename {
                self.filename = Some(arg.clone());
            } else {
                return Err("only one filename allowed".into());
            }
        }

        if let None = self.filename {
            Err("no filename given".into())
        } else {
            Ok(())
        }
    }

    pub fn filename(&self) -> &str {
        self.filename.as_ref().unwrap()
    }
}
