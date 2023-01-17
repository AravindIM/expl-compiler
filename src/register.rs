use std::error::Error;

pub struct RegPool {
    pool: Vec<bool>,
}

impl RegPool {
    pub fn new() -> RegPool {
        RegPool {
            pool: vec![false; 20],
        }
    }

    pub fn get_free(&mut self) -> Result<usize, Box<dyn Error>> {
        let reg = self
            .pool
            .iter()
            .position(|&x| x == false)
            .ok_or(Box::<dyn Error>::from("ERROR: No set_free registers"))?;
        self.pool[reg] = true;
        Ok(reg)
    }

    pub fn get_used(&self) -> Result<Vec<usize>, Box<dyn Error>> {
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

    pub fn set_free(&mut self, reg: usize) {
        self.pool[reg] = false;
    }
}
