struct Stub {
    mem: CodeMemory,

}

impl Stub {
    fn new() -> Stub {
        CodeMemory {
            mem: CodeMemory::new()
        }
    }
}
