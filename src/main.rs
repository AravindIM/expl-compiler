use std::{env, fs, process};
use compiler::generator::code_gen;
use compiler::utils::fetch_filenames;

use lrlex::lrlex_mod;
use lrpar::lrpar_mod;

lrlex_mod!("lex.l");
lrpar_mod!("yacc.y");

fn main() {
    let lexerdef = lex_l::lexerdef();

    let args: Vec<String> = env::args().collect();

    const  DEFAULT_OUTPUT_FILE:&str = "output.xsm";

    let (input_file, output_file) = fetch_filenames(args, DEFAULT_OUTPUT_FILE);

    let source_code = fs::read_to_string(&input_file)
        .expect(&format!("ERROR: Cannot read the file '{}'!", &input_file));

    let lexer = lexerdef.lexer(&source_code);

    let (res, errs) = yacc_y::parse(&lexer);

    for e in &errs {
        println!("{}", e.pp(&lexer, &yacc_y::token_epp));
    }

    if !errs.is_empty() {
        process::exit(1);
    }

    if let Some(Ok(node)) = res {
        code_gen(&lexer, &node, &output_file).expect("ERROR: Code generation halted!");
    }
}
