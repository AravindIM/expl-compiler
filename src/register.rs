use std::vec;

use crate::exception::compiler::CompilerError;

#[derive(Debug, Clone, PartialEq)]
pub enum RegState {
    Free,
    Busy,
    Hold,
}

pub struct RegPool {
    pool: Vec<RegState>,
    saved: Vec<usize>,
}

impl RegPool {
    pub fn new() -> RegPool {
        RegPool {
            pool: vec![RegState::Free; 20],
            saved: vec![],
        }
    }

    pub fn fetch(&mut self) -> Result<usize, CompilerError> {
        let reg = self
            .pool
            .iter()
            .position(|ref x| *x.clone() == RegState::Free)
            .ok_or(CompilerError("ERROR: No free registers!".to_string()))?;
        self.pool[reg] = RegState::Busy;
        Ok(reg)
    }

    pub fn fetch_busy(&self) -> Vec<usize> {
        self.pool
            .iter()
            .enumerate()
            .filter_map(|(index, ref r)| (*r.clone() == RegState::Busy).then(|| index))
            .collect::<Vec<usize>>()
    }

    fn set_busy(&mut self, reg: usize) -> Result<(), CompilerError> {
        if self.pool[reg] == RegState::Hold {
            return Err(CompilerError(format!(
                "ERROR: Cannot use R{} as it is being held!",
                reg
            )));
        }
        self.pool[reg] = RegState::Busy;
        Ok(())
    }

    pub fn free(&mut self, reg: usize) -> Result<(), CompilerError> {
        if self.pool[reg] == RegState::Hold {
            return Err(CompilerError(format!(
                "ERROR: Cannot free R{} as it is being held!",
                reg
            )));
        }
        self.pool[reg] = RegState::Free;
        Ok(())
    }

    pub fn save_context(&mut self) -> Vec<usize> {
        self.saved = self.fetch_busy();
        for reg in self.saved.clone() {
            self.free(reg).unwrap();
        }
        self.saved.clone()
    }

    pub fn restore_context(&mut self) -> Vec<usize> {
        for reg in self.saved.clone() {
            self.set_busy(reg).unwrap();
        }
        self.saved.reverse();
        let saved = self.saved.clone();
        self.saved = vec![];
        saved
    }

    pub fn hold(&mut self, reg: usize) -> Result<(), CompilerError> {
        match self.pool[reg] {
            RegState::Free | RegState::Busy => {
                self.pool[reg] = RegState::Hold;
                Ok(())
            }
            RegState::Hold => Err(CompilerError(format!(
                "ERROR: Cannot hold R{} as it is already being held!",
                reg
            ))),
        }
    }

    pub fn release(&mut self, reg: usize) -> Result<(), CompilerError> {
        if self.pool[reg] == RegState::Busy {
            return Err(CompilerError(format!(
                "ERROR: Cannot release R{} as it is busy and not being held!",
                reg
            )));
        }
        self.pool[reg] = RegState::Free;
        Ok(())
    }

    pub fn release_as_busy(&mut self, reg: usize) -> Result<(), CompilerError> {
        if self.pool[reg] == RegState::Busy {
            return Err(CompilerError(format!(
                "ERROR: Cannot release R{} as it is busy and not being held!",
                reg
            )));
        }
        self.pool[reg] = RegState::Busy;
        Ok(())
    }
}
