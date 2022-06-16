use std::collections::HashSet;
use std::fs::File;
use std::io::{self, Write};

use crate::bytecode::{BytecodeInstruction, BytecodeReader, ConstPoolEntry};
use crate::cannon::{self, CompilationFlags};
use crate::compiler::codegen::CompilationData;
use crate::compiler::dora_entry_stub;
use crate::language::sem_analysis::{FctDefinition, FctDefinitionId, SemAnalysis};
use crate::language::ty::SourceTypeArray;
use crate::masm::CodeDescriptor;

pub fn build(sa: &SemAnalysis, main_fct_id: FctDefinitionId) {
    let reachable_fcts = discover_reachable_functions(sa, main_fct_id);

    write_file(sa, &reachable_fcts).expect("write failed");
}

fn write_file(
    sa: &SemAnalysis,
    reachable_fcts: &[(FctDefinitionId, SourceTypeArray)],
) -> io::Result<()> {
    let mut file = File::create("program.s")?;
    writeln!(&mut file, "\t.text")?;

    write_builtin_functions(sa, &mut file)?;
    write_reachable_functions(sa, &mut file, reachable_fcts)?;

    Ok(())
}

fn write_builtin_functions(sa: &SemAnalysis, file: &mut File) -> io::Result<()> {
    write_main(file)?;

    let code_descriptor = dora_entry_stub::generate(sa);
    write_fct(file, "_dora_entry_stub", code_descriptor)?;

    Ok(())
}

fn write_main(file: &mut File) -> io::Result<()> {
    writeln!(file, "\t.globl _main")?;
    writeln!(file, "\t.p2align 2")?;
    writeln!(file, "_main:")?;
    writeln!(file, "\t.cfi_startproc")?;
    writeln!(file, "\tstp x29, x30, [sp, #-16]!")?;
    writeln!(file, "\tmov x29, sp")?;
    writeln!(file, "\tmov x29, sp")?;
    writeln!(file, "\t.cfi_def_cfa w29, 16")?;
    writeln!(file, "\t.cfi_offset w30, -8")?;
    writeln!(file, "\t.cfi_offset w29, -16")?;
    writeln!(file, "\tbl _run_aot_program")?;
    writeln!(file, "\tmov w0, #0")?;
    writeln!(file, "\tldp	x29, x30, [sp], #16")?;
    writeln!(file, "\tret")?;
    writeln!(file, "\t.cfi_endproc")?;

    Ok(())
}

fn write_reachable_functions(
    sa: &SemAnalysis,
    file: &mut File,
    reachable_fcts: &[(FctDefinitionId, SourceTypeArray)],
) -> io::Result<()> {
    for (fct_id, type_params) in reachable_fcts {
        let fct = sa.fcts.idx(*fct_id);
        let fct = fct.read();
        let symbol = fct_symbol(sa, &*fct);

        let compilation_data = CompilationData {
            bytecode_fct: fct.bytecode.as_ref().expect("bytecode missing"),
            type_params,
            params: SourceTypeArray::with(fct.params_with_self().to_vec()),
            return_type: fct.return_type.clone(),
            has_variadic_parameter: fct.is_variadic,
            pos: fct.pos,
            emit_debug: false,
            emit_code_comments: false,
        };

        let code_descriptor = cannon::compile(sa, compilation_data, CompilationFlags::aot());
        write_fct(file, &symbol, code_descriptor)?;
    }

    Ok(())
}

fn fct_symbol(sa: &SemAnalysis, fct: &FctDefinition) -> String {
    let name = sa.interner.str(fct.name);
    format!("_dora_uf_{}", name)
}

fn write_fct(file: &mut File, name: &str, code_descriptor: CodeDescriptor) -> io::Result<()> {
    writeln!(file, "\t.globl {}", name)?;
    writeln!(file, "\t.p2align 4")?;
    writeln!(file, "{}:", name)?;
    writeln!(file, "\t.cfi_startproc")?;
    write_binary_buffer(file, &code_descriptor.code)?;
    writeln!(file, "\t.cfi_endproc")?;

    Ok(())
}

fn write_binary_buffer(file: &mut File, buffer: &[u8]) -> io::Result<()> {
    write!(
        file,
        "\t.byte 0x{:x}",
        buffer.first().expect("empty buffer")
    )?;

    for &value in buffer.iter().skip(1) {
        write!(file, ", 0x{:x}", value)?;
    }

    writeln!(file)?;

    Ok(())
}

fn discover_reachable_functions(
    sa: &SemAnalysis,
    main_fct_id: FctDefinitionId,
) -> Vec<(FctDefinitionId, SourceTypeArray)> {
    let mut fcts = ReachableFunction::new(sa);
    fcts.find_all(main_fct_id);
    fcts.reachable_fcts
}

struct ReachableFunction<'a> {
    sa: &'a SemAnalysis,
    worklist: Vec<(FctDefinitionId, SourceTypeArray)>,
    visited: HashSet<(FctDefinitionId, SourceTypeArray)>,
    reachable_fcts: Vec<(FctDefinitionId, SourceTypeArray)>,
}

impl<'a> ReachableFunction<'a> {
    fn new(sa: &'a SemAnalysis) -> ReachableFunction<'a> {
        ReachableFunction {
            sa,
            worklist: Vec::new(),
            visited: HashSet::new(),
            reachable_fcts: Vec::new(),
        }
    }

    fn find_all(&mut self, main_fct_id: FctDefinitionId) {
        self.push_worklist(main_fct_id, SourceTypeArray::empty());

        while let Some((fct_id, type_params)) = self.worklist.pop() {
            self.reachable_fcts.push((fct_id, type_params.clone()));
            self.discover_outgoing_calls(fct_id, type_params);
        }
    }

    fn discover_outgoing_calls(&mut self, fct_id: FctDefinitionId, _type_params: SourceTypeArray) {
        let fct = self.sa.fcts.idx(fct_id);
        let fct = fct.read();

        let bytecode_fct = fct.bytecode.as_ref().expect("bytecode missing");

        let mut reader = BytecodeReader::new(bytecode_fct.code());

        while let Some(inst) = reader.next() {
            match inst {
                BytecodeInstruction::InvokeStatic { fct: idx, dest: _ }
                | BytecodeInstruction::InvokeDirect { fct: idx, dest: _ } => {
                    let entry = bytecode_fct.const_pool(idx);
                    match entry {
                        ConstPoolEntry::Fct(fct_id, type_params) => {
                            self.push_worklist(*fct_id, type_params.clone());
                        }

                        _ => unreachable!(),
                    }
                }

                _ => {}
            }
        }
    }

    fn push_worklist(&mut self, fct_id: FctDefinitionId, type_params: SourceTypeArray) {
        if self.visited.insert((fct_id, type_params.clone())) {
            self.worklist.push((fct_id, type_params));
        }
    }
}
