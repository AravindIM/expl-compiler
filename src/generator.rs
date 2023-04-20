use crate::builder::symboltable::get_gst_tail;
use crate::enums::ast::Ast;
use crate::enums::exception::{AssignError, OpError};
use crate::enums::flow::FlowType;
use crate::enums::operator::OpType;
use crate::exception::compiler::CompilerError;
use crate::label::LabelManager;
use crate::register::RegPool;
use crate::{enums::dtype::DType, function::FnDef};

use std::{
    fs::{self, File},
    io::Write,
};

pub struct CodeGenerator {
    regpool: RegPool,
    labelmanager: LabelManager,
    stackstart: usize,
}

impl CodeGenerator {
    pub fn new() -> CodeGenerator {
        CodeGenerator {
            regpool: RegPool::new(),
            labelmanager: LabelManager::new(),
            stackstart: 4095,
        }
    }

    fn write_header(&self, object_file: &mut File) -> Result<(), CompilerError> {
        writeln!(object_file, "0")?;
        writeln!(object_file, "2056")?;
        writeln!(object_file, "0")?;
        writeln!(object_file, "0")?;
        writeln!(object_file, "0")?;
        writeln!(object_file, "0")?;
        writeln!(object_file, "0")?;
        writeln!(object_file, "0")?;
        writeln!(
            object_file,
            "MOV SP, {}",
            self.stackstart as isize + get_gst_tail()
        )?;
        writeln!(object_file, "MOV BP, SP")?;
        writeln!(object_file, "PUSH R0")?;
        writeln!(object_file, "CALL F0")?;
        writeln!(object_file, "POP R0")?;
        writeln!(object_file, "MOV R0, \"Exit\"")?;
        writeln!(object_file, "PUSH R0")?;
        writeln!(object_file, "PUSH R0")?;
        writeln!(object_file, "PUSH R0")?;
        writeln!(object_file, "PUSH R0")?;
        writeln!(object_file, "PUSH R0")?;
        writeln!(object_file, "CALL 0")?;
        Ok(())
    }

    fn precall(&mut self, object_file: &mut File) -> Result<(), CompilerError> {
        for used_reg in self.regpool.save_context() {
            writeln!(object_file, "PUSH R{}", used_reg)?;
        }
        Ok(())
    }

    fn postcall(&mut self, object_file: &mut File) -> Result<(), CompilerError> {
        for used_reg in self.regpool.restore_context() {
            writeln!(object_file, "POP R{}", used_reg)?;
        }
        Ok(())
    }

    pub fn generate(&mut self, flist: &Vec<FnDef>, filename: &str) -> Result<(), CompilerError> {
        let object_filename = filename.replace(".xsm", ".o");

        File::create(&object_filename)?;
        File::create(&filename)?;

        let mut object_file = File::options().append(true).open(&object_filename)?;
        let mut exec_file = File::options().write(true).open(&filename)?;

        self.write_header(&mut object_file)?;

        for func in flist {
            writeln!(object_file, "F{}:", func.flabel)?;
            writeln!(object_file, "PUSH BP")?;
            writeln!(object_file, "MOV BP, SP")?;
            // dbg!(func.lsize);
            if func.lsize > 0 {
                writeln!(object_file, "ADD SP, {}", func.lsize)?;
            }
            self.ast_to_code(&func.body, &mut object_file)?;
            if func.lsize > 0 {
                writeln!(object_file, "SUB SP, {}", func.lsize)?;
            }
            writeln!(object_file, "POP BP")?;
            writeln!(object_file, "RET")?;
        }
        let object_code = fs::read_to_string(&object_filename)?;
        self.labelmanager.generate_label_map(&object_code)?;
        let exec_code = self.labelmanager.translate_label(&object_code)?;
        write!(exec_file, "{}", exec_code)?;
        Ok(())
    }

    fn ast_to_code(
        &mut self,
        node: &Ast,
        object_file: &mut File,
    ) -> Result<Option<usize>, CompilerError> {
        // dbg!(node.clone());
        match node {
            Ast::Literal { dtype, value, .. } => {
                let reg1 = self.regpool.fetch()?;
                match dtype {
                    DType::Data(_) => writeln!(object_file, "MOV R{}, {}", reg1, value)?,
                    DType::Pointer(_) => writeln!(object_file, "MOV R{}, {}", reg1, value)?,
                    // _ => return Err(format!("ERROR: Invalid Constant!").into())
                }
                return Ok(Some(reg1));
            }
            Ast::Var { .. } => {
                let reg1 = self
                    .ast_to_code(&node.get_address()?, object_file)?
                    .unwrap();
                let reg2 = self.regpool.fetch()?;
                writeln!(object_file, "MOV R{}, [R{}]", reg2, reg1)?;
                self.regpool.free(reg1)?;
                return Ok(Some(reg2));
            }
            Ast::FnCall { flabel, args, .. } => {
                let ret_reg = self.regpool.fetch()?;
                self.regpool.hold(ret_reg)?;
                self.precall(object_file)?;
                if let Some(args) = args.clone() {
                    let mut args = *args;
                    args.reverse();
                    for arg in args {
                        let reg = self.ast_to_code(&arg, object_file)?.unwrap();
                        writeln!(object_file, "PUSH R{}", reg)?;
                        self.regpool.free(reg)?;
                    }
                }
                writeln!(object_file, "PUSH R{}", ret_reg)?;

                writeln!(object_file, "CALL F{}", flabel)?;
                writeln!(object_file, "POP R{}", ret_reg)?;
                if let Some(args) = args {
                    writeln!(object_file, "SUB SP, {}", args.len())?;
                }
                self.postcall(object_file)?;
                self.regpool.release_as_busy(ret_reg)?;

                Ok(Some(ret_reg))
            }
            Ast::NullProg { .. } => return Ok(None),
            Ast::Connector { lhs, rhs, .. } => {
                self.ast_to_code(&*lhs, object_file)?;
                self.ast_to_code(&*rhs, object_file)?;
                return Ok(None);
            }
            Ast::Read { variable, .. } => {
                if let Ast::Var { .. } = &**variable {
                    self.precall(object_file)?;
                    let reg1 = self
                        .ast_to_code(&variable.get_address()?, object_file)?
                        .unwrap();

                    let reg2 = self.regpool.fetch()?;
                    writeln!(object_file, "MOV R{}, \"Read\"", reg2)?;
                    writeln!(object_file, "PUSH R{}", reg2)?;
                    writeln!(object_file, "MOV R{}, -1", reg2)?;
                    writeln!(object_file, "PUSH R{}", reg2)?;
                    writeln!(object_file, "PUSH R{}", reg1)?;
                    writeln!(object_file, "ADD SP, 2")?;
                    writeln!(object_file, "CALL 0")?;
                    writeln!(object_file, "SUB SP, 5")?;

                    self.regpool.free(reg1)?;
                    self.regpool.free(reg2)?;

                    self.postcall(object_file)?;

                    return Ok(None);
                }
                return Err(CompilerError(
                    "ERROR: read() has invalid argument!".to_string(),
                ));
            }
            Ast::Write { expr, .. } => {
                self.precall(object_file)?;
                let reg1 = self.ast_to_code(&*expr, object_file)?.unwrap();
                let reg2 = self.regpool.fetch()?;
                writeln!(object_file, "MOV R{}, \"Write\"", reg2)?;
                writeln!(object_file, "PUSH R{}", reg2)?;
                writeln!(object_file, "MOV R{}, -2", reg2)?;
                writeln!(object_file, "PUSH R{}", reg2)?;
                writeln!(object_file, "PUSH R{}", reg1)?;
                writeln!(object_file, "ADD SP, 2")?;
                writeln!(object_file, "CALL 0")?;
                writeln!(object_file, "SUB SP, 5")?;
                
                self.regpool.free(reg2)?;
                self.regpool.free(reg1)?;

                self.postcall(object_file)?;

                return Ok(None);
            }
            Ast::AsgStmt { variable, expr, .. } => {
                match variable.as_ref() {
                    Ast::Var { .. } => {
                        // *dtype = expr.get_type();
                        let reg1 = self.ast_to_code(&*expr, object_file)?.unwrap();
                        let reg2 = self
                            .ast_to_code(&variable.get_address()?, object_file)?
                            .unwrap();
                        writeln!(object_file, "MOV [R{}], R{}", reg2, reg1)?;
                        self.regpool.free(reg1)?;
                        self.regpool.free(reg2)?;
                        Ok(None)
                    }
                    Ast::Op { optype, lhs, .. } => {
                        if *optype == OpType::Deref {
                            let reg1 = self.ast_to_code(&*expr, object_file)?.unwrap();
                            let reg2 = self.ast_to_code(lhs, object_file)?.unwrap();
                            writeln!(object_file, "MOV [R{}], R{}", reg2, reg1)?;
                            self.regpool.free(reg1)?;
                            self.regpool.free(reg2)?;
                            return Ok(None);
                        }
                        Err(CompilerError::assign(AssignError::InvalidOp {
                            operation: optype.clone(),
                        }))
                    }
                    _ => Err(CompilerError::assign(AssignError::MissingLhs)),
                }
            }
            Ast::FlowStmt {
                ftype,
                bool_exprs,
                slists,
                ..
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

                    let bool_reg = self.ast_to_code(&*bool_expr, object_file)?.unwrap();

                    let if_slist_end_label = self.labelmanager.get_free_label();
                    writeln!(object_file, "JZ R{}, L{}", bool_reg, if_slist_end_label)?;

                    self.regpool.free(bool_reg)?;

                    self.ast_to_code(&*if_slist, object_file)?;

                    match else_slist {
                        Some(else_stmts) => {
                            let else_end_label = self.labelmanager.get_free_label();
                            writeln!(object_file, "JMP L{}", else_end_label)?;
                            writeln!(object_file, "L{}:", if_slist_end_label)?;
                            self.ast_to_code(&*else_stmts, object_file)?;
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

                    let bool_reg = self.ast_to_code(&*bool_expr, object_file)?.unwrap();

                    writeln!(object_file, "JZ R{}, L{}", bool_reg, while_end_label)?;

                    self.regpool.free(bool_reg)?;

                    self.ast_to_code(&*slist, object_file)?;

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
                    // self.labelmanager.push_label(do_start_label);

                    let do_condition_label = self.labelmanager.get_free_label();
                    self.labelmanager.push_label(do_condition_label);

                    let do_end_label = self.labelmanager.get_free_label();
                    self.labelmanager.push_label(do_end_label);

                    writeln!(object_file, "L{}:", do_start_label)?;

                    self.ast_to_code(&*slist, object_file)?;

                    writeln!(object_file, "L{}:", do_condition_label)?;

                    let bool_reg = self.ast_to_code(&*bool_expr, object_file)?.unwrap();

                    writeln!(object_file, "JNZ R{}, L{}", bool_reg, do_start_label)?;
                    writeln!(object_file, "L{}:", do_end_label)?;

                    self.regpool.free(bool_reg)?;

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
                    // self.labelmanager.push_label(repeat_start_label);

                    let repeat_condition_label = self.labelmanager.get_free_label();
                    self.labelmanager.push_label(repeat_condition_label);

                    let repeat_end_label = self.labelmanager.get_free_label();
                    self.labelmanager.push_label(repeat_end_label);

                    writeln!(object_file, "L{}:", repeat_start_label)?;

                    self.ast_to_code(&*slist, object_file)?;

                    writeln!(object_file, "L{}:", repeat_condition_label)?;

                    let bool_reg = self.ast_to_code(&*bool_expr, object_file)?.unwrap();

                    writeln!(object_file, "JZ R{}, L{}", bool_reg, repeat_start_label)?;
                    writeln!(object_file, "L{}:", repeat_end_label)?;

                    self.regpool.free(bool_reg)?;

                    self.labelmanager.pop_label()?;
                    self.labelmanager.pop_label()?;
                    Ok(None)
                }
            },
            Ast::Op {
                optype, lhs, rhs, ..
            } => {
                match rhs {
                    Some(rhs) => {
                        let reg1 = self.ast_to_code(&*lhs, object_file)?.unwrap();
                        let reg2 = self.ast_to_code(&*rhs, object_file)?.unwrap();
                        match optype {
                            OpType::Mod => {
                                writeln!(object_file, "MOD R{}, R{}", reg1, reg2)?;
                                self.regpool.free(reg2)?;
                                return Ok(Some(reg1));
                            }
                            OpType::Div => {
                                writeln!(object_file, "DIV R{}, R{}", reg1, reg2)?;
                                self.regpool.free(reg2)?;
                                return Ok(Some(reg1));
                            }
                            OpType::Mul => {
                                writeln!(object_file, "MUL R{}, R{}", reg1, reg2)?;
                                self.regpool.free(reg2)?;
                                return Ok(Some(reg1));
                            }
                            OpType::Add => {
                                writeln!(object_file, "ADD R{}, R{}", reg1, reg2)?;
                                self.regpool.free(reg2)?;
                                return Ok(Some(reg1));
                            }
                            OpType::Sub => {
                                writeln!(object_file, "SUB R{}, R{}", reg1, reg2)?;
                                self.regpool.free(reg2)?;
                                return Ok(Some(reg1));
                            }
                            OpType::Lt => {
                                writeln!(object_file, "LT R{}, R{}", reg1, reg2)?;
                                self.regpool.free(reg2)?;
                                return Ok(Some(reg1));
                            }
                            OpType::Gt => {
                                writeln!(object_file, "GT R{}, R{}", reg1, reg2)?;
                                self.regpool.free(reg2)?;
                                return Ok(Some(reg1));
                            }
                            OpType::Eq => {
                                writeln!(object_file, "EQ R{}, R{}", reg1, reg2)?;
                                self.regpool.free(reg2)?;
                                return Ok(Some(reg1));
                            }
                            OpType::NEq => {
                                writeln!(object_file, "NE R{}, R{}", reg1, reg2)?;
                                self.regpool.free(reg2)?;
                                return Ok(Some(reg1));
                            }
                            OpType::LEq => {
                                writeln!(object_file, "LE R{}, R{}", reg1, reg2)?;
                                self.regpool.free(reg2)?;
                                return Ok(Some(reg1));
                            }
                            OpType::GEq => {
                                writeln!(object_file, "GE R{}, R{}", reg1, reg2)?;
                                self.regpool.free(reg2)?;
                                return Ok(Some(reg1));
                            }
                            OpType::And => {
                                writeln!(object_file, "ADD R{}, R{}", reg1, reg2)?;
                                writeln!(object_file, "DIV R{}, 2", reg1)?;
                                self.regpool.free(reg2)?;
                                return Ok(Some(reg1));
                            }
                            OpType::Or => {
                                writeln!(object_file, "ADD R{}, R{}", reg1, reg2)?;
                                writeln!(object_file, "ADD R{}, 1", reg1)?;
                                writeln!(object_file, "DIV R{}, 2", reg1)?;
                                self.regpool.free(reg2)?;
                                return Ok(Some(reg1));
                            }
                            _ => {
                                return Err(CompilerError::op(OpError::Unary {
                                    operation: optype.clone(),
                                }))
                            }
                        }
                    }
                    None => {
                        match optype {
                            OpType::Amp => {
                                let reg1: usize;
                                // dbg!(*lhs.clone());
                                match *lhs.clone() {
                                    Ast::Var {
                                        address, is_local, ..
                                    } => {
                                        // dbg!(name);
                                        // dbg!(address.clone());
                                        // dbg!(is_local);
                                        reg1 = self.ast_to_code(&address, object_file)?.unwrap();
                                        if is_local {
                                            writeln!(object_file, "ADD R{}, BP", reg1)?;
                                        } else {
                                            writeln!(object_file, "ADD R{}, {}", reg1, self.stackstart)?;
                                        }
                                        // dbg!("getting address parsed");
                                    }
                                    _ => {
                                        return Err(CompilerError::op(OpError::NotVar {
                                            operation: optype.clone(),
                                        }));
                                    }
                                }
                                return Ok(Some(reg1));
                            }
                            OpType::Deref => {
                                let reg1 = self.ast_to_code(&*lhs, object_file)?.unwrap();
                                let reg2 = self.regpool.fetch()?;
                                writeln!(object_file, "MOV R{}, [R{}]", reg2, reg1)?;
                                self.regpool.free(reg1)?;
                                Ok(Some(reg2))
                            }
                            _ => {
                                return Err(CompilerError::op(OpError::Binary {
                                    operation: optype.clone(),
                                }))
                            }
                        }
                    }
                }
                // return Err(format!("ERROR: Operator {:?} has mismatching operands!", optype).into());
            }
            Ast::ReturnStmt { expr, .. } => {
                let reg1 = self.ast_to_code(&*expr, object_file)?.unwrap();
                let reg2 = self.regpool.fetch()?;
                writeln!(object_file, "MOV R{}, {}", reg2, "BP")?;
                writeln!(object_file, "SUB R{}, {}", reg2, 2)?;
                writeln!(object_file, "MOV [R{}], R{}", reg2, reg1)?;
                self.regpool.free(reg1)?;
                self.regpool.free(reg2)?;
                Ok(None)
            }
        }
    }
}
