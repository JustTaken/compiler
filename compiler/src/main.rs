use std::env;

use compiler::parser::Parser;
use mem::{allocate, deallocate, Arena};

fn main() {
    let start = std::time::Instant::now();
    let mut args = env::args();

    if args.len() != 3 {
        panic!("Program expect 2 arguments \"input path\" and \"output path\"");
    }

    let buffer = allocate(3);
    let mut arena = Arena::new(buffer);

    let program_name = args.next().unwrap();
    let input_path = args.next().unwrap();
    let output_path = args.next().unwrap();

    let mut parser = Parser::new(&input_path, &output_path, &mut arena);

    while parser.next() {}

    deallocate(buffer);

    println!(
        "took: {} nanos, to run {}",
        start.elapsed().as_nanos(),
        program_name
    );
}
