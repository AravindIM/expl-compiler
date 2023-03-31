pub mod errors;
pub mod generator;
pub mod label;
pub mod register;
pub mod symboltable;
pub mod tnode;
pub mod utils;
use std::sync::Mutex;

use lazy_static::lazy_static;
pub use symboltable::SymbolTable;

lazy_static! {
    pub static ref ST: Mutex<SymbolTable> = Mutex::new(SymbolTable::new());
    pub static ref LST: Mutex<SymbolTable> = Mutex::new(SymbolTable::new());
}
