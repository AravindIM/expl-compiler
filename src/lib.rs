use std::{error::Error, fs::File, io::Write};

use lrlex::DefaultLexeme;
use lrpar::NonStreamingLexer;

#[derive(Debug)]
pub enum Op {
    Div {
        lhs: Box<Tnode>,
        rhs: Box<Tnode>,
    },
    Mul {
        lhs: Box<Tnode>,
        rhs: Box<Tnode>,
    },
    Add {

        lhs: Box<Tnode>,
        rhs: Box<Tnode>,
    },
    Sub {
        lhs: Box<Tnode>,
        rhs: Box<Tnode>,
    },
}

#[derive(Debug)]
pub enum Tnode {
    Op(Op),
    Num {
        value: i32,
    },
}

struct RegPool {
    pool: Vec<bool>,
}

impl RegPool {
    fn new() -> RegPool {
        RegPool {
            pool: vec![false; 20],
        }
    }

    fn get_free(&mut self) -> Result<usize, Box<dyn Error>> {
        let reg = self
            .pool
            .iter()
            .position(|&x| x == false)
            .ok_or(Box::<dyn Error>::from("No free registers"))?;
        self.pool[reg] = true;
        Ok(reg)
    }

    fn set_free(&mut self, reg: usize) {
        self.pool[reg] = false;
    }
}

pub fn fetch_filenames(arglist: Vec<String>, default_output: &str) -> (String, String) {
    let def_output = String::from(default_output);
    let input_file = arglist.get(1).expect("ERROR: Please enter a source file!");
    let output_file = arglist.get(2).unwrap_or_else(|| &&def_output);
    (String::from(input_file), String::from(output_file))
}

fn write_header(out_file: &mut File) -> Result<(), Box<dyn Error>> {
    writeln!(out_file, "0")?;
    writeln!(out_file, "2056")?;
    writeln!(out_file, "0")?;
    writeln!(out_file, "0")?;
    writeln!(out_file, "0")?;
    writeln!(out_file, "0")?;
    writeln!(out_file, "0")?;
    writeln!(out_file, "0")?;
    writeln!(out_file, "MOV SP, 4095")?;
    Ok(())
}

fn write_tail(out_file: &mut File) -> Result<(), Box<dyn Error>> {
    writeln!(out_file, "MOV R0, \"Exit\"")?;
    writeln!(out_file, "PUSH R0")?;
    writeln!(out_file, "PUSH R0")?;
    writeln!(out_file, "PUSH R0")?;
    writeln!(out_file, "PUSH R0")?;
    writeln!(out_file, "PUSH R0")?;
    writeln!(out_file, "CALL 0")?;
    Ok(())
}

pub fn code_gen(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    node: Tnode,
    filename: &str,
) -> Result<(), Box<dyn Error>> {
    let mut regpool = RegPool::new();

    File::create(filename)?;

    let mut out_file = File::options().append(true).open(filename)?;

    write_header(&mut out_file)?;
    ast_to_code(lexer, node, &mut regpool, &mut out_file)?;
    write_tail(&mut out_file)?;
    Ok(())
}

fn ast_to_code(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    node: Tnode,
    regpool: &mut RegPool,
    out_file: &mut File,
) -> Result<usize, Box<dyn Error>> {
    match node {
        Tnode::Op(operator) => match operator {
            Op::Div { lhs, rhs } => {
                let reg1 = ast_to_code(lexer, *lhs, regpool, out_file)?;
                let reg2 = ast_to_code(lexer, *rhs, regpool, out_file)?;
                writeln!(out_file, "DIV R{}, R{}", reg1, reg2)?;
                regpool.set_free(reg2);
                return Ok(reg1);
            }
            Op::Mul { lhs, rhs } => {
                let reg1 = ast_to_code(lexer, *lhs, regpool, out_file)?;
                let reg2 = ast_to_code(lexer, *rhs, regpool, out_file)?;
                writeln!(out_file, "MUL R{}, R{}", reg1, reg2)?;
                regpool.set_free(reg2);
                return Ok(reg1);
            }
            Op::Add { lhs, rhs } => {
                let reg1 = ast_to_code(lexer, *lhs, regpool, out_file)?;
                let reg2 = ast_to_code(lexer, *rhs, regpool, out_file)?;
                writeln!(out_file, "ADD R{}, R{}", reg1, reg2)?;
                regpool.set_free(reg2);
                return Ok(reg1);
            }
            Op::Sub { lhs, rhs } => {
                let reg1 = ast_to_code(lexer, *lhs, regpool, out_file)?;
                let reg2 = ast_to_code(lexer, *rhs, regpool, out_file)?;
                writeln!(out_file, "SUB R{}, R{}", reg1, reg2)?;
                regpool.set_free(reg2);
                return Ok(reg1);
            }
        },
        Tnode::Num { value } => {
            let reg1 = regpool.get_free()?;
            writeln!(out_file, "MOV R{}, {}", reg1, value)?;
            return Ok(reg1);
        }
    }
}
