pub fn fetch_filenames(arglist: Vec<String>, default_output: &str) -> (String, String) {
    let def_output = String::from(default_output);
    let input_file = arglist.get(1).expect("ERROR: Please enter a source file!");
    let output_file = arglist.get(2).unwrap_or_else(|| &&def_output);
    (String::from(input_file), String::from(output_file))
}