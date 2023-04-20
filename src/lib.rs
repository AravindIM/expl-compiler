pub mod builder;
pub mod generator;
pub mod label;
pub mod register;
pub mod symboltable;
pub mod utils;
use std::sync::Mutex;
pub mod enums;
pub mod exception;
pub mod function;

use lazy_static::lazy_static;
pub use symboltable::SymbolTable;

lazy_static! {
    pub static ref GST: Mutex<SymbolTable> = Mutex::new(SymbolTable::new());
    pub static ref LST: Mutex<SymbolTable> = Mutex::new(SymbolTable::new());
}
