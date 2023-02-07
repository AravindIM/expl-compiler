use crate::ST;
use crate::label::LabelManager;
use crate::register::RegPool;
use crate::tnode::{FlowType, OpType, Tnode, DType, Primitive};
use lrlex::DefaultLexeme;
use lrpar::NonStreamingLexer;

use std::{
    error::Error,
    fs::{self, File},
    io::Write,
};

pub struct CodeGenerator {
    regpool: RegPool,
    labelmanager: LabelManager,
}

impl CodeGenerator {
    pub fn new() -> CodeGenerator {
        CodeGenerator{regpool: RegPool::new(), labelmanager: LabelManager::new()}
    }

    fn write_header(&self, object_file: &mut File) -> Result<(), Box<dyn Error>> {
        writeln!(object_file, "0")?;
        writeln!(object_file, "2056")?;
        writeln!(object_file, "0")?;
        writeln!(object_file, "0")?;
        writeln!(object_file, "0")?;
        writeln!(object_file, "0")?;
        writeln!(object_file, "0")?;
        writeln!(object_file, "0")?;
        writeln!(object_file, "MOV SP, {}", ST.lock().unwrap().get_bp())?;
        Ok(())
    }
    
    fn write_tail(&self, object_file: &mut File) -> Result<(), Box<dyn Error>> {
        writeln!(object_file, "MOV R0, \"Exit\"")?;
        writeln!(object_file, "PUSH R0")?;
        writeln!(object_file, "PUSH R0")?;
        writeln!(object_file, "PUSH R0")?;
        writeln!(object_file, "PUSH R0")?;
        writeln!(object_file, "PUSH R0")?;
        writeln!(object_file, "CALL 0")?;
        Ok(())
    }

    pub fn generate(
        &mut self,
        lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
        node: &Tnode,
        filename: &str,
    ) -> Result<(), Box<dyn Error>> {
        let object_filename = filename.replace(".xsm", ".o");
    
        File::create(&object_filename)?;
        File::create(&filename)?;
    
        let mut object_file = File::options().append(true).open(&object_filename)?;
        let mut exec_file = File::options().write(true).open(&filename)?;
    
        self.write_header(&mut object_file)?;
        self.ast_to_code(
            lexer,
            &node,
            &mut object_file,
        )?;
        self.write_tail(&mut object_file)?;
        let object_code = fs::read_to_string(&object_filename)
            .expect(&format!("ERROR: Unable to read object file {}!", &filename));
        self.labelmanager.generate_label_map(&object_code)?;
        let exec_code = self.labelmanager.translate_label(&object_code)?;
        write!(exec_file, "{}", exec_code)?;
        Ok(())
    }
    
    fn ast_to_code(
        &mut self,
        lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
        node: &Tnode,
        object_file: &mut File,
    ) -> Result<Option<usize>, Box<dyn Error>> {
        dbg!(node.clone());
        match node {
            Tnode::NullProg {span: _} => return Ok(None),
            Tnode::Connector {span: _, lhs, rhs } => {
                self.ast_to_code(lexer, &*lhs, object_file)?;
                self.ast_to_code(lexer, &*rhs, object_file)?;
                return Ok(None);
            }
            Tnode::Read {span: _, id } => {
                if let Tnode::Id {span: _, dtype: _, name: _, address } = &**id {
                    let reg1 = self.ast_to_code(lexer, &address, object_file)?.unwrap();
                    let used_reg_list = self.regpool.get_used()?;
                    for used_reg in used_reg_list.iter() {
                        writeln!(object_file, "PUSH R{}", used_reg)?;
                    }
                    let reg2 = self.regpool.get_free()?;
                    writeln!(object_file, "MOV R{}, \"Read\"", reg2)?;
                    writeln!(object_file, "PUSH R{}", reg2)?;
                    writeln!(object_file, "MOV R{}, -1", reg2)?;
                    writeln!(object_file, "PUSH R{}", reg2)?;
                    writeln!(object_file, "PUSH R{}", reg1)?;
                    writeln!(object_file, "PUSH R{}", reg2)?;
                    writeln!(object_file, "PUSH R{}", reg2)?;
                    writeln!(object_file, "CALL 0")?;
                    writeln!(object_file, "POP R{}", reg2)?;
                    writeln!(object_file, "POP R{}", reg1)?;
                    writeln!(object_file, "POP R{}", reg1)?;
                    writeln!(object_file, "POP R{}", reg1)?;
                    writeln!(object_file, "POP R{}", reg1)?;
                    self.regpool.set_free(reg1);
                    self.regpool.set_free(reg2);
                    for used_reg in used_reg_list.iter() {
                        writeln!(object_file, "POP R{}", used_reg)?;
                    }
                    return Ok(None);
                }
                return Err("ERROR: read() has invalid argument!".into());
            }
            Tnode::Write {span: _, expr } => {
                if let Some(reg1) = self.ast_to_code(lexer, &*expr, object_file)? {
                    let used_reg_list = self.regpool.get_used()?;
                    for used_reg in used_reg_list.iter() {
                        writeln!(object_file, "PUSH R{}", used_reg)?;
                    }
                    let reg2 = self.regpool.get_free()?;
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
                    self.regpool.set_free(reg2);
                    self.regpool.set_free(reg1);
                    for used_reg in used_reg_list.iter() {
                        writeln!(object_file, "POP R{}", used_reg)?;
                    }
                    return Ok(None);
                }
                return Err("ERROR: write() has invalid argument!".into());
            }
            Tnode::AsgStmt { span: _, id, expr } => {
                match id.as_ref() {
                    Tnode::Id {span: _, dtype:_, name: _, address } => {
                        // *dtype = expr.get_type();
                        let reg1 = self.ast_to_code(lexer, &address, object_file)?.unwrap();
                        let reg2 = self.ast_to_code(lexer, &*expr, object_file)?.unwrap();
                        writeln!(object_file, "MOV [R{}], R{}", reg1, reg2)?;
                        self.regpool.set_free(reg1);
                        self.regpool.set_free(reg2);
                        Ok(None)
                    }
                    Tnode::Op { span: _, dtype:_, optype, lhs, .. } => {
                        if *optype == OpType::Deref {
                            let reg1 = self.ast_to_code(lexer, lhs, object_file)?.unwrap();
                            let reg2 = self.ast_to_code(lexer, &*expr, object_file)?.unwrap();
                            writeln!(object_file, "MOV [R{}], R{}", reg1, reg2)?;
                            self.regpool.set_free(reg1);
                            self.regpool.set_free(reg2);
                            return Ok(None);
                        }
                        Err("ERROR: Incorrect Operation found in LHS of assignment".into())
                    }
                    _ => Err("ERROR: Missing identifier or dereference in lhs for assignment".into())
                }
            }
            Tnode::FlowStmt {
                span: _,
                ftype,
                bool_exprs,
                slists,
            } => match ftype {
                FlowType::Continue => {
                    let loop_end_label = self.labelmanager.pop_label()?;
                    let loop_start_label = self.labelmanager.pop_label()?;
                    writeln!(object_file, "JMP L{}", loop_start_label)?;
                    self.labelmanager.push_label(loop_start_label);
                    self.labelmanager.push_label(loop_end_label);
                    Ok(None)
                }
                FlowType::Break => {
                    let loop_end_label = self.labelmanager.pop_label()?;
                    let loop_start_label = self.labelmanager.pop_label()?;
                    writeln!(object_file, "JMP L{}", loop_end_label)?;
                    self.labelmanager.push_label(loop_start_label);
                    self.labelmanager.push_label(loop_end_label);
                    Ok(None)
                }
                FlowType::If => {
                    let bool_exprs = bool_exprs.as_ref().unwrap();
                    let bool_expr = bool_exprs.get(0).unwrap();
                    let slists = slists.as_ref().unwrap();
                    let if_slist = slists.get(0).unwrap();
                    let else_slist = slists.get(1);
    
                    let bool_reg =
                        self.ast_to_code(lexer, &*bool_expr, object_file)?.unwrap();
    
                    let if_slist_end_label = self.labelmanager.get_free_label();
                    writeln!(object_file, "JZ R{}, L{}", bool_reg, if_slist_end_label)?;
    
                    self.regpool.set_free(bool_reg);
    
                    self.ast_to_code(lexer, &*if_slist, object_file)?;
    
                    match else_slist {
                        Some(else_stmts) => {
                            let else_end_label = self.labelmanager.get_free_label();
                            writeln!(object_file, "JMP L{}", else_end_label)?;
                            writeln!(object_file, "L{}:", if_slist_end_label)?;
                            self.ast_to_code(lexer, &*else_stmts, object_file)?;
                            writeln!(object_file, "L{}:", else_end_label)?;
                            return Ok(None);
                        }
                        None => {
                            writeln!(object_file, "L{}:", if_slist_end_label)?;
                        }
                    }
    
                    Ok(None)
                }
                FlowType::While => {
                    let bool_exprs = bool_exprs.as_ref().unwrap();
                    let bool_expr = bool_exprs.get(0).unwrap();
                    let slists = slists.as_ref().unwrap();
                    let slist = slists.get(0).unwrap();
    
                    let while_start_label = self.labelmanager.get_free_label();
                    self.labelmanager.push_label(while_start_label);
    
                    let while_end_label = self.labelmanager.get_free_label();
                    self.labelmanager.push_label(while_end_label);
    
                    writeln!(object_file, "L{}:", while_start_label)?;
    
                    let bool_reg =
                        self.ast_to_code(lexer, &*bool_expr, object_file)?.unwrap();
    
                    writeln!(object_file, "JZ R{}, L{}", bool_reg, while_end_label)?;
    
                    self.regpool.set_free(bool_reg);
    
                    self.ast_to_code(lexer, &*slist, object_file)?;
    
                    writeln!(object_file, "JMP L{}", while_start_label)?;
    
                    writeln!(object_file, "L{}:", while_end_label)?;
    
                    self.labelmanager.pop_label()?;
                    self.labelmanager.pop_label()?;
                    Ok(None)
                }
                FlowType::DoWhile => {
                    let bool_exprs = bool_exprs.as_ref().unwrap();
                    let bool_expr = bool_exprs.get(0).unwrap();
                    let slists = slists.as_ref().unwrap();
                    let slist = slists.get(0).unwrap();
    
                    let do_start_label = self.labelmanager.get_free_label();
                    self.labelmanager.push_label(do_start_label);
    
                    let do_end_label = self.labelmanager.get_free_label();
                    self.labelmanager.push_label(do_end_label);
    
                    writeln!(object_file, "L{}:", do_start_label)?;
    
                    self.ast_to_code(lexer, &*slist, object_file)?;
    
                    let bool_reg =
                        self.ast_to_code(lexer, &*bool_expr, object_file)?.unwrap();
    
                    writeln!(object_file, "JNZ R{}, L{}", bool_reg, do_start_label)?;
                    writeln!(object_file, "L{}:", do_end_label)?;
    
                    self.regpool.set_free(bool_reg);
    
                    self.labelmanager.pop_label()?;
                    self.labelmanager.pop_label()?;
                    Ok(None)
                }
                FlowType::RepeatUntil => {
                    let bool_exprs = bool_exprs.as_ref().unwrap();
                    let bool_expr = bool_exprs.get(0).unwrap();
                    let slists = slists.as_ref().unwrap();
                    let slist = slists.get(0).unwrap();
    
                    let repeat_start_label = self.labelmanager.get_free_label();
                    self.labelmanager.push_label(repeat_start_label);
    
                    let repeat_end_label = self.labelmanager.get_free_label();
                    self.labelmanager.push_label(repeat_end_label);
    
                    writeln!(object_file, "L{}:", repeat_start_label)?;
    
                    self.ast_to_code(lexer, &*slist, object_file)?;
    
                    let bool_reg =
                        self.ast_to_code(lexer, &*bool_expr, object_file)?.unwrap();
    
                    writeln!(object_file, "JZ R{}, L{}", bool_reg, repeat_start_label)?;
                    writeln!(object_file, "L{}:", repeat_end_label)?;
    
                    self.regpool.set_free(bool_reg);
    
                    self.labelmanager.pop_label()?;
                    self.labelmanager.pop_label()?;
                    Ok(None)
                }
            },
            Tnode::Id {span: _, dtype: _, name: _, address } => {
                // dbg!(name.clone());
                // let id = var.name;
                let reg1 = self.ast_to_code(lexer, &address, object_file)?.unwrap();
                let reg2 = self.regpool.get_free()?;
                writeln!(object_file, "MOV R{}, [R{}]", reg2, reg1)?;
                self.regpool.set_free(reg1);
                return Ok(Some(reg2));
            }
            Tnode::Op {
                span: _,
                dtype: _,
                optype,
                lhs,
                rhs,
            } => {
                match rhs {
                    Some(rhs) => {
                        let reg1 = self.ast_to_code(lexer, &*lhs, object_file)?.unwrap();
                        let reg2 = self.ast_to_code(lexer, &*rhs, object_file)?.unwrap();
                        match optype {
                            OpType::Mod => {
                                writeln!(object_file, "MOD R{}, R{}", reg1, reg2)?;
                                self.regpool.set_free(reg2);
                                return Ok(Some(reg1));
                            }
                            OpType::Div => {
                                writeln!(object_file, "DIV R{}, R{}", reg1, reg2)?;
                                self.regpool.set_free(reg2);
                                return Ok(Some(reg1));
                            }
                            OpType::Mul => {
                                writeln!(object_file, "MUL R{}, R{}", reg1, reg2)?;
                                self.regpool.set_free(reg2);
                                return Ok(Some(reg1));
                            }
                            OpType::Add => {
                                writeln!(object_file, "ADD R{}, R{}", reg1, reg2)?;
                                self.regpool.set_free(reg2);
                                return Ok(Some(reg1));
                            }
                            OpType::Sub => {
                                writeln!(object_file, "SUB R{}, R{}", reg1, reg2)?;
                                self.regpool.set_free(reg2);
                                return Ok(Some(reg1));
                            }
                            OpType::Lt => {
                                writeln!(object_file, "LT R{}, R{}", reg1, reg2)?;
                                self.regpool.set_free(reg2);
                                return Ok(Some(reg1));
                            }
                            OpType::Gt => {
                                writeln!(object_file, "GT R{}, R{}", reg1, reg2)?;
                                self.regpool.set_free(reg2);
                                return Ok(Some(reg1));
                            }
                            OpType::Eq => {
                                writeln!(object_file, "EQ R{}, R{}", reg1, reg2)?;
                                self.regpool.set_free(reg2);
                                return Ok(Some(reg1));
                            }
                            OpType::NEq => {
                                writeln!(object_file, "NE R{}, R{}", reg1, reg2)?;
                                self.regpool.set_free(reg2);
                                return Ok(Some(reg1));
                            }
                            OpType::LEq => {
                                writeln!(object_file, "LE R{}, R{}", reg1, reg2)?;
                                self.regpool.set_free(reg2);
                                return Ok(Some(reg1));
                            }
                            OpType::GEq => {
                                writeln!(object_file, "GE R{}, R{}", reg1, reg2)?;
                                self.regpool.set_free(reg2);
                                return Ok(Some(reg1));
                            }
                            _ => {
                                return Err("ERROR: Unary Operator got two operands!".into())
                            }
                        }
                    }
                    None => {
                        match optype {
                            OpType::Amp => {
                                let reg1: usize;
                                match *lhs.clone() {
                                    Tnode::Id { span:_, dtype:_, name:_, address } => {
                                        // dbg!(address.clone());
                                        reg1 = self.ast_to_code(lexer, &address, object_file)?.unwrap();
                                        // dbg!("getting address parsed");
                                    }
                                    _ => return Err("ERROR: & operator can only be used on identifiers".into())
                                }
                                return Ok(Some(reg1));
                            },
                            OpType::Deref => {
                                let reg1 = self.ast_to_code(lexer, &*lhs, object_file)?.unwrap();
                                let reg2 = self.regpool.get_free()?;
                                writeln!(object_file, "MOV R{}, [R{}]", reg2, reg1)?;
                                self.regpool.set_free(reg1);
                                Ok(Some(reg2))
                            }
                            _ => return Err("ERROR: Binary Operator only got one operand".into())
                        }
                    }
                }
                // return Err(format!("ERROR: Operator {:?} has mismatching operands!", optype).into());
            }
            Tnode::Constant {span: _, dtype, value } => {
                let reg1 = self.regpool.get_free()?;
                match dtype {
                    DType::Data(Primitive::Int) => writeln!(object_file, "MOV R{}, {}", reg1, value)?,
                    DType::Data(Primitive::Str) => writeln!(object_file, "MOV R{}, {}", reg1, value)?,
                    DType::Pointer(Primitive::Int) => writeln!(object_file, "MOV R{}, {}", reg1, value)?,
                    DType::Pointer(Primitive::Str) => writeln!(object_file, "MOV R{}, {}", reg1, value)?,
                    _ => return Err(format!("ERROR: Invalid Constant!").into())
                }
                return Ok(Some(reg1));
            }
        }
    }
}

