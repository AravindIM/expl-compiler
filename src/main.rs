use std::{env, fs, process};
use compiler::errors::LangParseError;
use compiler::generator::CodeGenerator;
use compiler::utils::fetch_filenames;

use lrlex::lrlex_mod;
use lrpar::{lrpar_mod, NonStreamingLexer};

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

    let mut code_gen = CodeGenerator::new();

    for e in &errs {
        println!("{}", e.pp(&lexer, &yacc_y::token_epp));
    }

    if !errs.is_empty() {
        process::exit(1);
    }

    match res {
        Some(parse_res) => match parse_res {
            Ok(node) => code_gen.generate(&lexer, &node, &output_file).expect("ERROR: Code generation halted!"),
            Err(LangParseError(span, message)) => {
                let ((start_line, start_col), (_, _)) = lexer.line_col(span);
                eprintln!("ERROR: Parsing error at line {start_line} column {start_col}:");
                eprintln!("{message}");
            }
        }
        None => eprintln!("ERROR: Parsing failed!")
    }
}
