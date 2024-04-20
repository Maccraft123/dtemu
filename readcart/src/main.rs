use clap::Parser;
use std::fs;
use std::path::PathBuf;

#[derive(Debug, Parser)]
struct Cli {
    file: PathBuf,
}

fn main() {
    let args = Cli::parse();
    let input_bytes = fs::read(args.file)
        .expect("Failed to read the input file");
    let tmp = cartridge::INesFile::parse(&input_bytes).unwrap();
    println!("{:#?}", tmp.1.header());
}
