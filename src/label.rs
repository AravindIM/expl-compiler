use std::collections::HashMap;
use regex::Regex;
use std::error::Error;

pub struct LabelManager {
    current_insert: usize,
    current_map: usize,
    label_map: HashMap<String, usize>,
    label_stack: Vec<usize>,
}

impl LabelManager {
    pub fn new() -> LabelManager {
        LabelManager {
            current_insert: 0,
            current_map: 0,
            label_map: HashMap::new(),
            label_stack: vec![],
        }
    }

    pub fn get_free_label(&mut self) -> usize {
        let label = self.current_insert;
        self.current_insert += 1;
        label
    }

    pub fn push_label(&mut self, label: usize) {
        self.label_stack.push(label);
    }

    pub fn pop_label(&mut self) -> Result<usize, Box<dyn Error>> {
        self.label_stack
            .pop()
            .ok_or(Box::<dyn Error>::from("ERROR: Label stack is empty!"))
    }

    pub fn generate_label_map(&mut self, object_code: &str) -> Result<(), Box<dyn Error>> {
        let label_regex = Regex::new(r"(L[0-9]+):")?;
        for (index, code) in object_code.lines().enumerate() {
            if let Some(label_captures) = label_regex.captures(code) {
                if let Some(label) = label_captures.get(1) {
                    self.label_map.insert(
                        String::from(label.as_str()),
                        2056 + 2 * (index - self.current_map - 8),
                    );
                    // println!("index: {} count: {}", index, self.count);
                    self.current_map += 1;
                }
            }
        }
        Ok(())
    }

    pub fn translate_label(&self, object_code: &str) -> Result<String, Box<dyn Error>> {
        let label_regex = Regex::new(r"L[0-9]+:\n")?;
        let mut out_code = label_regex.replace_all(object_code, "").to_string();
        for (key, value) in self.label_map.iter() {
            out_code = out_code.replace(key, format!("{value}").as_str());
        }
        Ok(out_code)
    }
}