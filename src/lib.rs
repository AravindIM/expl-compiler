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
pub struct Id {
    pub name: String,
}

impl Id {
    fn get_address(&self) -> Result<u32, Box<dyn Error>> {
        Ok(self.name.chars()
            .nth(0)
            .ok_or(Box::<dyn Error>::from("Invalid variable"))? as u32
            - 'a' as u32
            + 4096)
    }
}

#[derive(Debug)]
pub enum Tnode {
    NullProg,
    Op(Op),
    Num {
        value: i32,
    },
    Id(Id),
    Connector {
        lhs: Box<Tnode>,
        rhs: Box<Tnode>,
    },
    Read {
        id: Box<Tnode>,
    },
    Write {
        expr: Box<Tnode>,
    },
    AsgStmt {
        id: Box<Tnode>,
        expr: Box<Tnode>,
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
            .ok_or(Box::<dyn Error>::from("No set_free registers"))?;
        self.pool[reg] = true;
        Ok(reg)
    }

    fn get_used(&self) -> Result<Vec<usize>, Box<dyn Error>> {
        Ok(self
            .pool
            .iter()
            .enumerate()
            .filter_map(|(index, &r)| (r == true).then(|| index))
            .collect::<Vec<usize>>())
    }

    // fn set_used(&mut self, reg: usize) {
    //     self.pool[reg] = true;
    // }

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
    writeln!(out_file, "MOV SP, {}", 4096+26)?;
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
        Tnode::NullProg => return Ok(0),
        Tnode::Id(var) => {
            // let id = var.name;
            let address: u32 = var.get_address()?;
            let reg1 = regpool.get_free()?;
            let reg2 = regpool.get_free()?;
            writeln!(out_file, "MOV R{}, {}", reg1, address)?;
            writeln!(out_file, "MOV R{}, [R{}]", reg2, reg1)?;
            regpool.set_free(reg1);
            return Ok(reg2);
        }
        Tnode::Connector { lhs, rhs } => {
            let reg1 = ast_to_code(lexer, *lhs, regpool, out_file)?;
            let reg2 = ast_to_code(lexer, *rhs, regpool, out_file)?;
            regpool.set_free(reg1);
            regpool.set_free(reg2);
            let reg3 = regpool.get_free()?;
            return Ok(reg3);
        }
        Tnode::Read { id } => {
            if let Tnode::Id(var) = *id {
                let address: u32 = var.get_address()?;
                let used_reg_list = regpool.get_used()?;
                for used_reg in used_reg_list.iter() {
                    writeln!(out_file, "PUSH R{}", used_reg)?;
                }
                let reg1 = regpool.get_free()?;
                let reg2 = regpool.get_free()?;
                writeln!(out_file, "MOV R{}, \"Read\"", reg1)?;
                writeln!(out_file, "PUSH R{}", reg1)?;
                writeln!(out_file, "MOV R{}, -1", reg1)?;
                writeln!(out_file, "PUSH R{}", reg1)?;
                writeln!(out_file, "MOV R{}, {}", reg1, address)?;
                writeln!(out_file, "PUSH R{}", reg1)?;
                writeln!(out_file, "PUSH R{}", reg2)?;
                writeln!(out_file, "PUSH R{}", reg2)?;
                writeln!(out_file, "CALL 0")?;
                writeln!(out_file, "POP R{}", reg1)?;
                writeln!(out_file, "POP R{}", reg2)?;
                writeln!(out_file, "POP R{}", reg2)?;
                writeln!(out_file, "POP R{}", reg2)?;
                writeln!(out_file, "POP R{}", reg2)?;
                regpool.set_free(reg2);
                for used_reg in used_reg_list.iter() {
                    writeln!(out_file, "POP R{}", used_reg)?;
                }
                return Ok(reg1);
            }
            let reg1 = regpool.get_free()?;
            return Ok(reg1);
        }
        Tnode::Write { expr } => {
            let reg1 = ast_to_code(lexer, *expr, regpool, out_file)?;
            let used_reg_list = regpool.get_used()?;
            for used_reg in used_reg_list.iter() {
                writeln!(out_file, "PUSH R{}", used_reg)?;
            }
            let reg2 = regpool.get_free()?;
            writeln!(out_file, "MOV R{}, \"Write\"", reg2)?;
            writeln!(out_file, "PUSH R{}", reg2)?;
            writeln!(out_file, "MOV R{}, -2", reg2)?;
            writeln!(out_file, "PUSH R{}", reg2)?;
            writeln!(out_file, "PUSH R{}", reg1)?;
            writeln!(out_file, "PUSH R{}", reg2)?;
            writeln!(out_file, "PUSH R{}", reg2)?;
            writeln!(out_file, "CALL 0")?;
            writeln!(out_file, "POP R{}", reg1)?;
            writeln!(out_file, "POP R{}", reg2)?;
            writeln!(out_file, "POP R{}", reg2)?;
            writeln!(out_file, "POP R{}", reg2)?;
            writeln!(out_file, "POP R{}", reg2)?;
            regpool.set_free(reg2);
            for used_reg in used_reg_list.iter() {
                writeln!(out_file, "POP R{}", used_reg)?;
            }
            return Ok(reg1);
        }
        Tnode::AsgStmt { id, expr } => {
            if let Tnode::Id(var) = *id {
                let address = var.get_address()?;
                let reg1 = ast_to_code(lexer, *expr, regpool, out_file)?;
                let reg2 = regpool.get_free()?;
                writeln!(out_file, "MOV R{}, {}", reg2, address)?;
                writeln!(out_file, "MOV [R{}], R{}", reg2, reg1)?;
            };
            let reg3 = regpool.get_free()?;
            return Ok(reg3);
        }
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
