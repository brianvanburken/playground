use std::borrow::Borrow;
use std::fs;
use std::io::{Read, Write, SeekFrom, Seek};
use std::{path::PathBuf};
use edit::{edit_file, Builder};
use color_eyre::{eyre::WrapErr, owo_colors::OwoColorize, Result};

const TEMPLATE: &[u8;2] = b"# ";

pub fn write(garden_path: PathBuf, title: Option<String>) -> Result<()> {
    let (mut file, filepath) = Builder::new()
        .suffix(".md")
        .rand_bytes(5)
        .tempfile_in(&garden_path)
        .wrap_err("Failed to create WIP file")?
        .keep()
        .wrap_err("Failed to keep tempfile")?;
    file.write_all(TEMPLATE)
        .wrap_err("Unable to write tempalte")?;
    edit_file(&filepath)?;
    let mut contents = String::new();
    file.seek(SeekFrom::Start(0))?;
    file.read_to_string(&mut contents)?;

    let document_title = title.or_else(|| {
        contents
            .lines()
            .find(|v| v.starts_with("# "))
            .map(|maybe_line| {
                maybe_line
                .trim_start_matches("# ")
                .to_string()
            })
    });

    let filename = match document_title {
       Some(raw_title) => confirm_filename(&raw_title),
       None => ask_for_filename(),
    }?;

    let mut i: usize = 0;
    loop {
        let dest_filename = format!(
            "{}{}",
            filename,
            if i == 0 {
                "".to_string()
            } else {
                i.to_string()
            }
        );
        let mut dest = garden_path.join(dest_filename);
        dest.set_extension("md");
        if dest.exists() {
            i = i + 1;
        } else {
            fs::rename(filepath, &dest)?;
            break;
        }
    }
    Ok(())
}

fn ask_for_filename() -> Result<String> {
    rprompt::prompt_reply_stderr(&format!(
        "{}",
        "\nEnter filename\n> ".blue().bold())
    )
    .wrap_err("Failed to get filename")
    .map(|title| slug::slugify(title))
}

fn confirm_filename(raw_title: &str) -> Result<String> {
    loop {
        let result = rprompt::prompt_reply_stderr(
            &format!(
                "\n{} `{}`\n{}",
                "Current title:".green().bold(),
                raw_title,
                "Do you want a different title? (y/N): ".blue().bold(),
            ),
        )
        .wrap_err("Failed to get input for y/n question")?;

        match result.as_str() {
            "y" | "Y" => break ask_for_filename(),
            "n" | "N" | "" => break Ok(slug::slugify(raw_title)),
            _ => {} // ask again
        }
    }
}
