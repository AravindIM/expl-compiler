pub mod errors;
pub mod generator;
pub mod tnode;
pub mod register;
pub mod label;
pub mod utils;
pub mod symboltable;
use std::sync::Mutex;

use lazy_static::lazy_static;
pub use symboltable::{SymbolTable, Declaration};

lazy_static!(
    pub static ref ST: Mutex<SymbolTable> = Mutex::new(SymbolTable::new());
);