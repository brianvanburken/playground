use assert_cmd::Command;
use color_eyre::{eyre::Result};
use assert_fs::prelude::*;
use predicates::prelude::*;

#[test]
fn test_help() -> Result<()> {
    let mut cmd = Command::cargo_bin("garden")?;
    let assert = cmd.arg("--help").assert();
    assert.success().stderr("");
    Ok(())
}

#[test]
fn test_write_help() -> Result<()> {
    let mut cmd = Command::cargo_bin("garden")?;
    let assert = cmd.arg("write").arg("--help").assert();
    assert.success().stderr("");
    Ok(())
}

#[test]
fn test_write_with_title() {
    let (mut cmd, temp_dir) = setup_command();

    let assert = cmd
        .arg("write")
        .arg("-t")
        .arg("atitle")
        .write_stdin("N\n".as_bytes())
        .assert();

    assert.success();

    temp_dir
        .child("atitle.md")
        .assert(predicate::path::exists());
}

#[test]
fn test_write_with_written_title() {
    let (mut cmd, temp_dir) = setup_command();

    let assert = cmd
        .arg("write")
        .write_stdin("N\n".as_bytes())
        .assert();

    assert.success();

    temp_dir
        .child("testing.md")
        .assert(predicate::path::exists());
}

fn setup_command() -> (Command, assert_fs::TempDir) {
    let temp_dir = assert_fs::TempDir::new().unwrap();

    let mut cmd = Command::cargo_bin("garden").unwrap();

    let fake_editor_path = std::env::current_dir()
        .expect("expect to be in a dir")
        .join("tests")
        .join("fake_editor.sh");
    if !fake_editor_path.exists() {
        panic!("fake editor shell script could not be found.")
    }

    cmd
        .env("EDITOR", fake_editor_path.into_os_string())
        .env("GARDEN_PATH", temp_dir.path());

    (cmd, temp_dir)
}
