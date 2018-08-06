use std::fs;
use std::mem;
use std::path::Path;

use baseline;
use class::TypeParams;
use ctxt::{exception_get_and_clear, Fct, FctId, SemContext};
use dora_parser::ast::Import;
use dora_parser::ast::{self, Ast};
use dora_parser::error::msg::Msg;
use dora_parser::interner::Interner;
use dora_parser::lexer::position::Position;
use dora_parser::lexer::reader::Reader;
use driver::cmd;
use exception::DoraToNativeInfo;
use object::{self, Handle, Testing};
use os;

use dora_parser::ast::{Class, Const, Function, Trait};

use dora_parser::ast::Elem::*;
use dora_parser::parser::{NodeIdGenerator, Parser};
use semck;
use semck::specialize::specialize_class_id;
use ty::BuiltinType;

pub fn parse_import(import: &mut Import, ast0: &mut Ast,interner0: &mut Interner,id_gen: &NodeIdGenerator) {
    let mut ast = Ast::new();
    let mut interner = Interner::new();
    let mut path2 = import.import_from.clone();
    parse_dir("stdlib", &id_gen, &mut ast, &mut interner);
    path2.push_str(".dora");
    let path = Path::new(&path2);

    if path.is_file() {
        parse_file(&path2, &id_gen, &mut ast, &mut interner);
    } else if path.is_dir() {
        panic!("Imports from directory is impossible for now :(");
    } else {
        println!("file `{}` does not exist.", &import.import_from);
        panic!("");
    }

    for file in ast.files.clone().iter_mut() {
        for elem in file.elements.iter_mut() {
            match elem {
                ElemInclude(ref mut include) => {
                    include.path.push_str(".dora");
                    parse_file(&include.path, &id_gen, &mut ast, &mut interner).unwrap();
                }
                _ => (),
            }
        }
    }

    let mut original_file = ast0.files[5].clone();
    
    for import_name in import.to_import.clone().iter() {
        let name = interner.intern(import_name);
        for file in ast.files.clone().iter_mut() {
            for elem in ast.files[5].elements.clone().iter_mut() {
                match elem {
                    ElemFunction(ref fun) => {
                       
                       println!("{}",interner.str(name));
                       println!("{}",interner.str(fun.name));
                        if interner.str(fun.name) == interner.str(name) {
                            
                            let mut edited_fun = fun.clone();
                            edited_fun.id = id_gen.next();
                            ast0.files[5].elements.push(ElemFunction(edited_fun));
                        } else {
                            return;
                        }
                    }
                    _ => {()}
                }
            }
        }
    }
    println!("{:?}",ast);
    
}




pub fn start() -> i32 {
    let args = cmd::parse();
    if !args.cmd_init {
        if args.flag_version {
            println!("dora v0.01b");
            return 0;
        }

        let mut interner = Interner::new();
        let id_generator = NodeIdGenerator::new();
        let mut ast = Ast::new();

        if let Err(code) =
            parse_dir("stdlib", &id_generator, &mut ast, &mut interner).and_then(|_| {
                let path = Path::new(&args.arg_file);

                if path.is_file() {
                    parse_file(&args.arg_file, &id_generator, &mut ast, &mut interner)
                } else if path.is_dir() {
                    parse_dir(&args.arg_file, &id_generator, &mut ast, &mut interner)
                } else {
                    println!("file or directory `{}` does not exist.", &args.arg_file);
                    Err(1)
                }
            }) {
            return code;
        }

        if args.flag_emit_ast {
            ast::dump::dump(&ast, &interner);
        }

        

        for file in ast.files.clone().iter_mut() {
            for elem in file.elements.iter_mut() {
                match elem {
                    ElemInclude(ref mut include) => {
                        include.path.push_str(".dora");
                        parse_file(&include.path, &id_generator, &mut ast, &mut interner).unwrap();
                    }

                    _ => (),
                }
            }
        }

        for import in ast.imports.clone().iter_mut() {
            parse_import(import, &mut ast,&mut interner, &id_generator);
        }

        let mut ctxt = SemContext::new(args, &ast, interner);
        semck::check(&mut ctxt, None);

        // register signal handler
        os::register_signals(&ctxt);

        let main = if ctxt.args.cmd_test {
            None
        } else {
            find_main(&ctxt)
        };

        if ctxt.diag.borrow().has_errors() {
            ctxt.diag.borrow().dump();
            let no_errors = ctxt.diag.borrow().errors().len();

            if no_errors == 1 {
                println!("{} error found.", no_errors);
            } else {
                println!("{} errors found.", no_errors);
            }

            return 1;
        }

        // if --check given, stop after type/semantic check
        if ctxt.args.flag_check {
            return 0;
        }

        if ctxt.args.cmd_test {
            run_tests(&ctxt)
        } else {
            run_main(&ctxt, main.unwrap())
        }
    } else {
        use std::env;
        use std::fs;
        use std::path::PathBuf;
        let current_directory = env::current_dir().unwrap_or(PathBuf::from(env!("PWD")));
        let cur_dir_str = current_directory.to_str().unwrap();
        let project_name = args.arg_name.clone();
        let full_path = format!("{}/{}", cur_dir_str, project_name);

        let cargo_toml = format!(
            "[package]\n
name=\"{}\"\n
version=\"0.0.1\"\n
authors = [\"\"]\n
[dependencies.dora]\n 
git = \"https://github.com/pigprogrammer/dora\"\n 
            ",
            project_name
        );

        let main_rs = format!(
            "#[macro_use]extern crate dora;\n
use dora::start;
use std::process::exit;
use std::path::Path;
use dora::driver::cmd;
fn main() {{\n
\texit(start(Path::new(\"src/main.dora\"),None,cmd::Args::default()));\n
}}\n"
        );
        if let Err(err) = fs::create_dir(full_path.clone()) {
            panic!(format!("Cannot create directory;\nError: {}\n\n", err));
        }
        if let Err(err) = fs::create_dir(format!("{}/src", full_path)) {
            panic!(format!("Cannot create directory;\nError: {}\n\n", err));
        }
        if let Err(err) = fs::File::create(format!("{}/Cargo.toml", &full_path.clone())) {
            panic!(format!("Cannot create file;\nError: {}\n\n", err));
        }
        if let Err(err) = fs::File::create(format!("{}/src/main.rs", &full_path.clone())) {
            panic!(format!("Cannot create file;\nError: {}\n\n", err));
        }
        if let Err(err) = fs::File::create(format!("{}/src/main.dora", &full_path.clone())) {
            panic!(format!("Cannot create file;\nError: {}\n\n", err));
        }

        fs::write(format!("{}/Cargo.toml", full_path), cargo_toml.as_bytes()).unwrap();
        fs::write(format!("{}/src/main.rs", full_path), main_rs.as_bytes()).unwrap();

        use std::os;
        if let Err(err) = os::unix::fs::symlink(
            format!("{}/stdlib", env!("CARGO_MANIFEST_DIR")),
            format!("{}/stdlib", full_path),
        ) {
            panic!(format!(
                "Unable to symlink stdlib directory!\n\nError: {}\n",
                err
            ));
        }
        return 1;
    }
}

pub fn run_tests<'ast>(ctxt: &SemContext<'ast>) -> i32 {
    let mut tests = 0;
    let mut passed = 0;

    for fct in ctxt.fcts.iter() {
        let fct = fct.borrow();

        if !is_test_fct(ctxt, &*fct) {
            continue;
        }

        tests += 1;

        print!("test {} ... ", ctxt.interner.str(fct.name));

        if run_test(ctxt, fct.id) {
            passed += 1;
            println!("ok");
        } else {
            println!("failed");
        }
    }

    println!(
        "{} tests executed; {} passed; {} failed.",
        tests,
        passed,
        tests - passed
    );

    // if all tests passed exit with 0, otherwise 1
    if tests == passed {
        0
    } else {
        1
    }
}

pub fn run_test<'ast>(ctxt: &SemContext<'ast>, fct: FctId) -> bool {
    let fct_ptr = {
        let mut dtn = DoraToNativeInfo::new();
        let type_params = TypeParams::empty();

        ctxt.use_dtn(&mut dtn, || {
            baseline::generate(&ctxt, fct, &type_params, &type_params)
        })
    };

    let testing_class = ctxt.vips.testing_class;
    let testing_class = specialize_class_id(ctxt, testing_class);
    let testing = object::alloc(ctxt, testing_class).cast();

    let fct: extern "C" fn(Handle<Testing>) -> i32 = unsafe { mem::transmute(fct_ptr) };

    // execute test
    fct(testing);

    // see if test failed with exception
    let exception = exception_get_and_clear();

    exception.is_null() && !testing.has_failed()
}

pub fn is_test_fct<'ast>(ctxt: &SemContext<'ast>, fct: &Fct<'ast>) -> bool {
    // tests need to be standalone functions, with no return type and a single parameter
    if !fct.parent.is_none() || !fct.return_type.is_unit() || fct.param_types.len() != 1 {
        return false;
    }

    // parameter needs to be of type Testing
    let testing_cls = ctxt.cls(ctxt.vips.testing_class);
    if fct.param_types[0] != testing_cls {
        return false;
    }

    // the functions name needs to start with `test`
    let fct_name = ctxt.interner.str(fct.name);
    fct_name.starts_with("test")
}

pub fn run_main<'ast>(ctxt: &SemContext<'ast>, main: FctId) -> i32 {
    let fct_ptr = {
        let mut dtn = DoraToNativeInfo::new();
        let type_params = TypeParams::empty();

        ctxt.use_dtn(&mut dtn, || {
            baseline::generate(&ctxt, main, &type_params, &type_params)
        })
    };

    let fct: extern "C" fn() -> i32 = unsafe { mem::transmute(fct_ptr) };
    let res = fct();

    let is_unit = ctxt.fcts[main].borrow().return_type.is_unit();

    // main-fct without return value exits with status 0
    if is_unit {
        0

    // else use return value of main for exit status
    } else {
        res
    }
}

pub fn parse_dir(
    dirname: &str,
    id_generator: &NodeIdGenerator,
    ast: &mut Ast,
    interner: &mut Interner,
) -> Result<(), i32> {
    let path = Path::new(dirname);

    if path.is_dir() {
        for entry in fs::read_dir(path).unwrap() {
            let path = entry.unwrap().path();

            if path.is_file() && path.extension().unwrap() == "dora" {
                parse_file(path.to_str().unwrap(), id_generator, ast, interner)?;
            }
        }

        Ok(())
    } else {
        println!("directory `{}` does not exist.", dirname);

        Err(1)
    }
}

pub fn parse_file(
    filename: &str,
    id_generator: &NodeIdGenerator,
    ast: &mut Ast,
    interner: &mut Interner,
) -> Result<(), i32> {
    let reader = if filename == "-" {
        match Reader::from_input() {
            Err(_) => {
                println!("unable to read from stdin.");
                return Err(1);
            }

            Ok(reader) => reader,
        }
    } else {
        match Reader::from_file(filename) {
            Err(_) => {
                println!("unable to read file `{}`", filename);
                return Err(1);
            }

            Ok(reader) => reader,
        }
    };

    if let Err(error) = Parser::new(reader, id_generator, ast, interner).parse() {
        println!("{}", error);
        println!("1 error found.");
        return Err(1);
    }

    Ok(())
}

pub fn find_main<'ast>(ctxt: &SemContext<'ast>) -> Option<FctId> {
    let name = ctxt.interner.intern("main");
    let fctid = match ctxt.sym.borrow().get_fct(name) {
        Some(id) => id,
        None => {
            ctxt.diag
                .borrow_mut()
                .report(Position::new(1, 1), Msg::MainNotFound);
            return None;
        }
    };

    let fct = ctxt.fcts[fctid].borrow();
    let ret = fct.return_type;

    if (ret != BuiltinType::Unit && ret != BuiltinType::Int) || fct.params_without_self().len() > 0
    {
        let pos = fct.ast.pos;
        ctxt.diag.borrow_mut().report(pos, Msg::WrongMainDefinition);
        return None;
    }

    Some(fctid)
}
