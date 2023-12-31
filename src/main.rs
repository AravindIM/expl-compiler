use compiler::exception::semantic::SemanticError;
use compiler::generator::CodeGenerator;
use compiler::utils::fetch_filenames;
use std::{env, fs, process};

use lrlex::lrlex_mod;
use lrpar::{lrpar_mod, NonStreamingLexer};

lrlex_mod!("lex.l");
lrpar_mod!("yacc.y");

fn main() {
    let lexerdef = lex_l::lexerdef();

    let args: Vec<String> = env::args().collect();

    const DEFAULT_OUTPUT_FILE: &str = "output.xsm";

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
            Ok(node) => {
                if let Err(e) = code_gen.generate(&node, &output_file) {
                    panic!("{}", e);
                }
            }
            Err(SemanticError(span, message)) => {
                let ((start_line, start_col), (_, _)) = lexer.line_col(span);
                eprintln!("ERROR: Semantic error at line {start_line} column {start_col}:");
                eprintln!("ERROR: {message}");
            }
        },
        None => eprintln!("ERROR: Parsing failed!"),
    }
}
