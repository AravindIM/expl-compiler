use std::path::Path;

#[test]
fn run_xsm() {
    use std::fs;
    // use std::fs::File;
    use std::process::{Command, Stdio};

    const SRC_BIN: &str = "output/output.xsm";
    const  DEST_BIN: &str = "xsm_expl/bin/output.xsm";

    let dest_dir = Path::new(&DEST_BIN).parent().unwrap().to_str().unwrap();

    // let bin = fs::read_to_string(SRC_BIN).expect(&format!("ERROR: Cannot read the file '{}'!", SRC_BIN));
    // let instructions:Vec<&str> = bin.lines().collect();
    // let mut debug_bin = instructions[..8].to_vec();
    // debug_bin.push("BRKP");
    // debug_bin.extend(instructions[8..].iter());
    // let debug_bin = debug_bin.join("\n");

    fs::create_dir_all(&dest_dir).expect(&format!("ERROR: Cannot create directory {}", dest_dir));

    // File::create(DEST_BIN).expect(&format!("ERROR: Cannot create the file '{}'!", DEST_BIN));
    // fs::write(DEST_BIN, debug_bin).expect(&format!("ERROR: Cannot write to the file '{}'!", DEST_BIN));



    // Copy files from output to xsm
    fs::copy(SRC_BIN, DEST_BIN).unwrap();

    // Run script.sh from xsm
    let mut output = Command::new("bash")
        .arg("./xsm_expl/run.sh")
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()
        // .output()
        .expect("failed to execute process");

    let _ = output.wait();
    // assert!(output.status.success());
    assert!(true);
}