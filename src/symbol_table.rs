struct SymbolTable {
    map: HashMap<String,Symbol>,
}

impl SymbolTable {
    fn new() -> SymbolTable {
        SymbolTable { map: HashMap::new() }
    }

    fn get(name: &str) -> &Symbol {

    }

    fn put(name: String, sym: Symbol) {

    }
}

enum Symbol {
    Type(String),
    Function(Function)
}

