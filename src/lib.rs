use regex::Regex;
use std::{
    collections::HashMap,
    error::Error,
    fs::{self, File},
    io::Write,
};

use lrlex::DefaultLexeme;
use lrpar::NonStreamingLexer;

#[derive(Debug)]
pub enum Op {
    Div { lhs: Box<Tnode>, rhs: Box<Tnode> },
    Mul { lhs: Box<Tnode>, rhs: Box<Tnode> },
    Add { lhs: Box<Tnode>, rhs: Box<Tnode> },
    Sub { lhs: Box<Tnode>, rhs: Box<Tnode> },
    LEq { lhs: Box<Tnode>, rhs: Box<Tnode> },
    GEq { lhs: Box<Tnode>, rhs: Box<Tnode> },
    Eq { lhs: Box<Tnode>, rhs: Box<Tnode> },
    NEq { lhs: Box<Tnode>, rhs: Box<Tnode> },
    Gt { lhs: Box<Tnode>, rhs: Box<Tnode> },
    Lt { lhs: Box<Tnode>, rhs: Box<Tnode> },
}

#[derive(Debug)]
pub struct Id {
    pub name: String,
}

impl Id {
    fn get_address(&self) -> Result<u32, Box<dyn Error>> {
        Ok(self
            .name
            .chars()
            .nth(0)
            .ok_or(Box::<dyn Error>::from("ERROR: Invalid variable"))? as u32
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
    IfStmt {
        bool_expr: Box<Tnode>,
        if_slist: Box<Tnode>,
        else_slist: Box<Option<Tnode>>,
    },
    WhileStmt {
        bool_expr: Box<Tnode>,
        slist: Box<Tnode>,
    },
    DoWhileStmt {
        bool_expr: Box<Tnode>,
        slist: Box<Tnode>,
    },
    RepeatUntilStmt {
        bool_expr: Box<Tnode>,
        slist: Box<Tnode>,
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
            .ok_or(Box::<dyn Error>::from("ERROR: No set_free registers"))?;
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

pub struct LabelPool {
    current: u32,
}

impl LabelPool {
    fn new() -> LabelPool {
        LabelPool { current: 0 }
    }

    fn get_label(&mut self) -> u32 {
        let label = self.current;
        self.current += 1;
        label
    }
}

pub struct LabelTranslator {
    count: usize,
    label_map: HashMap<String, usize>,
}

impl LabelTranslator {
    fn new() -> LabelTranslator {
        LabelTranslator {
            count: 0,
            label_map: HashMap::new(),
        }
    }

    fn fetch_labels(&mut self, object_code: &str) -> Result<(), Box<dyn Error>> {
        let label_regex = Regex::new(r"(L[0-9]+):")?;
        for (index, code) in object_code.lines().enumerate() {
            if let Some(label_captures) = label_regex.captures(code) {
                if let Some(label) = label_captures.get(1) {
                    self.label_map.insert(
                        String::from(label.as_str()),
                        2056 + 2 * (index + 1 - self.count - 8),
                    );
                    // println!("index: {} count: {}", index, self.count);
                    self.count += 1;
                }
            }
        }
        Ok(())
    }

    fn remove_labels(&self, object_code: &str) -> Result<String, Box<dyn Error>> {
        let label_regex = Regex::new(r"L[0-9]+:\n")?;
        Ok(label_regex.replace_all(object_code, "").to_string())
    }

    fn replace_label_ref(&self, object_code: &str) -> Result<String, Box<dyn Error>> {
        let mut out_code = object_code.to_string();
        for (key, value) in self.label_map.iter() {
            out_code = out_code.replace(key, format!("{value}").as_str());
        }
        Ok(out_code)
    }
}

pub fn fetch_filenames(arglist: Vec<String>, default_output: &str) -> (String, String) {
    let def_output = String::from(default_output);
    let input_file = arglist.get(1).expect("ERROR: Please enter a source file!");
    let output_file = arglist.get(2).unwrap_or_else(|| &&def_output);
    (String::from(input_file), String::from(output_file))
}

fn write_header(object_file: &mut File) -> Result<(), Box<dyn Error>> {
    writeln!(object_file, "0")?;
    writeln!(object_file, "2056")?;
    writeln!(object_file, "0")?;
    writeln!(object_file, "0")?;
    writeln!(object_file, "0")?;
    writeln!(object_file, "0")?;
    writeln!(object_file, "0")?;
    writeln!(object_file, "0")?;
    writeln!(object_file, "MOV SP, {}", 4096 + 26)?;
    Ok(())
}

fn write_tail(object_file: &mut File) -> Result<(), Box<dyn Error>> {
    writeln!(object_file, "MOV R0, \"Exit\"")?;
    writeln!(object_file, "PUSH R0")?;
    writeln!(object_file, "PUSH R0")?;
    writeln!(object_file, "PUSH R0")?;
    writeln!(object_file, "PUSH R0")?;
    writeln!(object_file, "PUSH R0")?;
    writeln!(object_file, "CALL 0")?;
    Ok(())
}

pub fn code_gen(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    node: Tnode,
    filename: &str,
) -> Result<(), Box<dyn Error>> {
    let mut regpool = RegPool::new();
    let mut labelpool = LabelPool::new();
    let object_filename = filename.replace(".xsm", ".o");
    let mut label_translator = LabelTranslator::new();

    File::create(&object_filename)?;
    File::create(&filename)?;

    let mut object_file = File::options().append(true).open(&object_filename)?;
    let mut exec_file = File::options().write(true).open(&filename)?;

    write_header(&mut object_file)?;
    ast_to_code(lexer, node, &mut regpool, &mut labelpool, &mut object_file)?;
    write_tail(&mut object_file)?;
    let object_code =
        fs::read_to_string(&object_filename).expect("ERROR: Unable to read object file {filename}");
    label_translator.fetch_labels(&object_code)?;
    let exec_code = label_translator.remove_labels(&object_code)?;
    let exec_code = label_translator.replace_label_ref(&exec_code)?;
    write!(exec_file, "{}", exec_code)?;
    Ok(())
}

fn ast_to_code(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    node: Tnode,
    regpool: &mut RegPool,
    labelpool: &mut LabelPool,
    object_file: &mut File,
) -> Result<Option<usize>, Box<dyn Error>> {
    match node {
        Tnode::NullProg => return Ok(None),
        Tnode::Connector { lhs, rhs } => {
            ast_to_code(lexer, *lhs, regpool, labelpool, object_file)?;
            ast_to_code(lexer, *rhs, regpool, labelpool, object_file)?;
            return Ok(None);
        }
        Tnode::Read { id } => {
            if let Tnode::Id(var) = *id {
                let address: u32 = var.get_address()?;
                let used_reg_list = regpool.get_used()?;
                for used_reg in used_reg_list.iter() {
                    writeln!(object_file, "PUSH R{}", used_reg)?;
                }
                let reg1 = regpool.get_free()?;
                let reg2 = regpool.get_free()?;
                writeln!(object_file, "MOV R{}, \"Read\"", reg1)?;
                writeln!(object_file, "PUSH R{}", reg1)?;
                writeln!(object_file, "MOV R{}, -1", reg1)?;
                writeln!(object_file, "PUSH R{}", reg1)?;
                writeln!(object_file, "MOV R{}, {}", reg1, address)?;
                writeln!(object_file, "PUSH R{}", reg1)?;
                writeln!(object_file, "PUSH R{}", reg2)?;
                writeln!(object_file, "PUSH R{}", reg2)?;
                writeln!(object_file, "CALL 0")?;
                writeln!(object_file, "POP R{}", reg1)?;
                writeln!(object_file, "POP R{}", reg2)?;
                writeln!(object_file, "POP R{}", reg2)?;
                writeln!(object_file, "POP R{}", reg2)?;
                writeln!(object_file, "POP R{}", reg2)?;
                regpool.set_free(reg2);
                regpool.set_free(reg1);
                for used_reg in used_reg_list.iter() {
                    writeln!(object_file, "POP R{}", used_reg)?;
                }
                return Ok(None);
            }
            return Err("ERROR: read() has invalid argument".into());
        }
        Tnode::Write { expr } => {
            if let Some(reg1) = ast_to_code(lexer, *expr, regpool, labelpool, object_file)? {
                let used_reg_list = regpool.get_used()?;
                for used_reg in used_reg_list.iter() {
                    writeln!(object_file, "PUSH R{}", used_reg)?;
                }
                let reg2 = regpool.get_free()?;
                writeln!(object_file, "MOV R{}, \"Write\"", reg2)?;
                writeln!(object_file, "PUSH R{}", reg2)?;
                writeln!(object_file, "MOV R{}, -2", reg2)?;
                writeln!(object_file, "PUSH R{}", reg2)?;
                writeln!(object_file, "PUSH R{}", reg1)?;
                writeln!(object_file, "PUSH R{}", reg2)?;
                writeln!(object_file, "PUSH R{}", reg2)?;
                writeln!(object_file, "CALL 0")?;
                writeln!(object_file, "POP R{}", reg1)?;
                writeln!(object_file, "POP R{}", reg2)?;
                writeln!(object_file, "POP R{}", reg2)?;
                writeln!(object_file, "POP R{}", reg2)?;
                writeln!(object_file, "POP R{}", reg2)?;
                regpool.set_free(reg2);
                regpool.set_free(reg1);
                for used_reg in used_reg_list.iter() {
                    writeln!(object_file, "POP R{}", used_reg)?;
                }
                return Ok(None);
            }
            return Err("ERROR: write() has invalid argument".into());
        }
        Tnode::AsgStmt { id, expr } => {
            if let Tnode::Id(var) = *id {
                let address = var.get_address()?;
                if let Some(reg1) = ast_to_code(lexer, *expr, regpool, labelpool, object_file)? {
                    let reg2 = regpool.get_free()?;
                    writeln!(object_file, "MOV R{}, {}", reg2, address)?;
                    writeln!(object_file, "MOV [R{}], R{}", reg2, reg1)?;
                }
            };
            return Ok(None);
        }
        Tnode::IfStmt {
            bool_expr,
            if_slist,
            else_slist,
        } => {
            if let Some(bool_reg) = ast_to_code(lexer, *bool_expr, regpool, labelpool, object_file)?
            {
                let if_slist_end_label = labelpool.get_label();
                writeln!(object_file, "JZ R{}, L{}", bool_reg, if_slist_end_label)?;

                regpool.set_free(bool_reg);

                ast_to_code(lexer, *if_slist, regpool, labelpool, object_file)?;

                match *else_slist {
                    Some(else_stmts) => {
                        let else_end_label = labelpool.get_label();
                        writeln!(object_file, "JMP L{}", else_end_label)?;
                        writeln!(object_file, "L{}:", if_slist_end_label)?;
                        ast_to_code(lexer, else_stmts, regpool, labelpool, object_file)?;
                        writeln!(object_file, "L{}:", else_end_label)?;
                        return Ok(None);
                    }
                    None => {
                        writeln!(object_file, "L{}:", if_slist_end_label)?;
                    }
                }
            }
            return Ok(None);
        }
        Tnode::WhileStmt { bool_expr, slist } => {
            let while_start_label = labelpool.get_label();
            writeln!(object_file, "L{}:", while_start_label)?;

            if let Some(bool_reg) = ast_to_code(lexer, *bool_expr, regpool, labelpool, object_file)?
            {
                let while_end_label = labelpool.get_label();

                writeln!(object_file, "JZ R{}, L{}", bool_reg, while_end_label)?;

                regpool.set_free(bool_reg);

                ast_to_code(lexer, *slist, regpool, labelpool, object_file)?;

                writeln!(object_file, "JMP L{}", while_start_label)?;

                writeln!(object_file, "L{}:", while_end_label)?;

                return Ok(None);
            }
            return Err("ERROR: while() has invalid argument".into());
        }
        Tnode::DoWhileStmt { bool_expr, slist } => {
            let do_label = labelpool.get_label();

            writeln!(object_file, "L{}:", do_label)?;

            ast_to_code(lexer, *slist, regpool, labelpool, object_file)?;

            if let Some(bool_reg) = ast_to_code(lexer, *bool_expr, regpool, labelpool, object_file)?
            {
                writeln!(object_file, "JNZ R{}, L{}", bool_reg, do_label)?;
                
                regpool.set_free(bool_reg);

                return Ok(None);
            }
            return Err("ERROR: do-while() has invalid argument".into());
        }
        Tnode::RepeatUntilStmt { bool_expr, slist } => {
            let do_label = labelpool.get_label();

            writeln!(object_file, "L{}:", do_label)?;

            ast_to_code(lexer, *slist, regpool, labelpool, object_file)?;

            if let Some(bool_reg) = ast_to_code(lexer, *bool_expr, regpool, labelpool, object_file)?
            {
                writeln!(object_file, "JZ R{}, L{}", bool_reg, do_label)?;
                
                regpool.set_free(bool_reg);

                return Ok(None);
            }
            return Err("ERROR: repeat-until() has invalid argument".into());
        }
        Tnode::Id(var) => {
            // let id = var.name;
            let address: u32 = var.get_address()?;
            let reg1 = regpool.get_free()?;
            let reg2 = regpool.get_free()?;
            writeln!(object_file, "MOV R{}, {}", reg1, address)?;
            writeln!(object_file, "MOV R{}, [R{}]", reg2, reg1)?;
            regpool.set_free(reg1);
            return Ok(Some(reg2));
        }
        Tnode::Op(operator) => match operator {
            Op::Div { lhs, rhs } => {
                if let Some(reg1) = ast_to_code(lexer, *lhs, regpool, labelpool, object_file)? {
                    if let Some(reg2) = ast_to_code(lexer, *rhs, regpool, labelpool, object_file)? {
                        writeln!(object_file, "DIV R{}, R{}", reg1, reg2)?;
                        regpool.set_free(reg2);
                        return Ok(Some(reg1));
                    }
                }
                return Err("ERROR: Operator / has mismatching operands".into());
            }
            Op::Mul { lhs, rhs } => {
                if let Some(reg1) = ast_to_code(lexer, *lhs, regpool, labelpool, object_file)? {
                    if let Some(reg2) = ast_to_code(lexer, *rhs, regpool, labelpool, object_file)? {
                        writeln!(object_file, "MUL R{}, R{}", reg1, reg2)?;
                        regpool.set_free(reg2);
                        return Ok(Some(reg1));
                    }
                }
                return Err("ERROR: Operator * has mismatching operands".into());
            }
            Op::Add { lhs, rhs } => {
                if let Some(reg1) = ast_to_code(lexer, *lhs, regpool, labelpool, object_file)? {
                    if let Some(reg2) = ast_to_code(lexer, *rhs, regpool, labelpool, object_file)? {
                        writeln!(object_file, "ADD R{}, R{}", reg1, reg2)?;
                        regpool.set_free(reg2);
                        return Ok(Some(reg1));
                    }
                }
                return Err("ERROR: Operator + has mismatching operands".into());
            }
            Op::Sub { lhs, rhs } => {
                if let Some(reg1) = ast_to_code(lexer, *lhs, regpool, labelpool, object_file)? {
                    if let Some(reg2) = ast_to_code(lexer, *rhs, regpool, labelpool, object_file)? {
                        writeln!(object_file, "SUB R{}, R{}", reg1, reg2)?;
                        regpool.set_free(reg2);
                        return Ok(Some(reg1));
                    }
                }
                return Err("ERROR: Operator - has mismatching operands".into());
            }
            Op::LEq { lhs, rhs } => {
                if let Some(reg1) = ast_to_code(lexer, *lhs, regpool, labelpool, object_file)? {
                    if let Some(reg2) = ast_to_code(lexer, *rhs, regpool, labelpool, object_file)? {
                        writeln!(object_file, "LE R{}, R{}", reg1, reg2)?;
                        regpool.set_free(reg2);
                        return Ok(Some(reg1));
                    }
                }
                return Err("ERROR: Operator <= has mismatching operands".into());
            }
            Op::GEq { lhs, rhs } => {
                if let Some(reg1) = ast_to_code(lexer, *lhs, regpool, labelpool, object_file)? {
                    if let Some(reg2) = ast_to_code(lexer, *rhs, regpool, labelpool, object_file)? {
                        writeln!(object_file, "GE R{}, R{}", reg1, reg2)?;
                        regpool.set_free(reg2);
                        return Ok(Some(reg1));
                    }
                }
                return Err("ERROR: Operator >= has mismatching operands".into());
            }
            Op::Eq { lhs, rhs } => {
                if let Some(reg1) = ast_to_code(lexer, *lhs, regpool, labelpool, object_file)? {
                    if let Some(reg2) = ast_to_code(lexer, *rhs, regpool, labelpool, object_file)? {
                        writeln!(object_file, "EQ R{}, R{}", reg1, reg2)?;
                        regpool.set_free(reg2);
                        return Ok(Some(reg1));
                    }
                }
                return Err("ERROR: Operator == has mismatching operands".into());
            }
            Op::NEq { lhs, rhs } => {
                if let Some(reg1) = ast_to_code(lexer, *lhs, regpool, labelpool, object_file)? {
                    if let Some(reg2) = ast_to_code(lexer, *rhs, regpool, labelpool, object_file)? {
                        writeln!(object_file, "NE R{}, R{}", reg1, reg2)?;
                        regpool.set_free(reg2);
                        return Ok(Some(reg1));
                    }
                }
                return Err("ERROR: Operator != has mismatching operands".into());
            }
            Op::Lt { lhs, rhs } => {
                if let Some(reg1) = ast_to_code(lexer, *lhs, regpool, labelpool, object_file)? {
                    if let Some(reg2) = ast_to_code(lexer, *rhs, regpool, labelpool, object_file)? {
                        writeln!(object_file, "LT R{}, R{}", reg1, reg2)?;
                        regpool.set_free(reg2);
                        return Ok(Some(reg1));
                    }
                }
                return Err("ERROR: Operator < has mismatching operands".into());
            }
            Op::Gt { lhs, rhs } => {
                if let Some(reg1) = ast_to_code(lexer, *lhs, regpool, labelpool, object_file)? {
                    if let Some(reg2) = ast_to_code(lexer, *rhs, regpool, labelpool, object_file)? {
                        writeln!(object_file, "GT R{}, R{}", reg1, reg2)?;
                        regpool.set_free(reg2);
                        return Ok(Some(reg1));
                    }
                }
                return Err("ERROR: Operator > has mismatching operands".into());
            }
        },
        Tnode::Num { value } => {
            let reg1 = regpool.get_free()?;
            writeln!(object_file, "MOV R{}, {}", reg1, value)?;
            return Ok(Some(reg1));
        }
    }
}
