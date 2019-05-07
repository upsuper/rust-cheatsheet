#[macro_use]
extern crate combine;

use crate::input::InputData;
use crate::page_gen::generate_to;
use std::env;
use std::error::Error;

mod input;
mod page_gen;
mod parser;
mod token;

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = env::args();
    let _ = args.next(); // executable path
                         // Get path of input data file and output file
    let input_file = args.next().expect("must specify input file");
    let output_file = args.next().expect("must specify output file");
    // Generate the page
    let input = InputData::from_file(&input_file)?;
    generate_to(&output_file, &input)?;
    Ok(())
}
