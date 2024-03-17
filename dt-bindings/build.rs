use bindgen::Builder;
use std::fs;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=c/");

    let iter = fs::read_dir("./c/")
        .unwrap()
        .flatten()
        .map(|d| d.path())
        .filter(|p| p.to_string_lossy().ends_with(".h"));
    let mut lib_rs = String::new();

    for file in iter {
        let out_path = file
            .with_extension("rs")
            .file_name()
            .unwrap()
            .to_string_lossy()
            .replace('-', "_");
        Builder::default()
            .use_core()
            .header(file.to_string_lossy())
            .generate()
            .expect("Failed to generate rust equivalents to dt-bindings")
            .write_to_file(&format!("src/{}", out_path))
            .expect("Failed to write rust bindings");
        lib_rs += &format!("pub mod {};", PathBuf::from(out_path).with_extension("").file_name().unwrap().to_string_lossy());
    }

    fs::write("src/lib.rs", lib_rs).unwrap();
}
