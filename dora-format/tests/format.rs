use dora_format::test_utils::check_source;
use std::fs;
use std::path::Path;

#[test]
#[ignore]
fn golden_files() {
    let data_dir = Path::new("tests/data");
    let entries = fs::read_dir(data_dir).expect("tests/data missing");
    let mut mismatches = 0;

    for entry in entries {
        let entry = entry.expect("read_dir entry");
        let input_path = entry.path();
        let file_name = input_path.file_name().and_then(|name| name.to_str());
        if !matches!(file_name, Some(name) if name.ends_with(".in.dora")) {
            continue;
        }

        println!("==== {}", input_path.display());

        let input_name = file_name.expect("input name");
        let stem = input_name.strip_suffix(".in.dora").expect("input stem");
        let output_path = data_dir.join(format!("{}.out.dora", stem));

        let input = fs::read_to_string(&input_path).expect("read input");
        let expected = fs::read_to_string(&output_path).expect("read expected output");
        if !check_source(&input, &expected) {
            mismatches += 1;
        }
    }

    if mismatches > 0 {
        panic!("{} golden file(s) mismatched", mismatches);
    }
}
