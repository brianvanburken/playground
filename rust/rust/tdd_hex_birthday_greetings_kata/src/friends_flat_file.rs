use std::{
    fs::File,
    io::{BufRead, BufReader},
};

use crate::*;

pub struct FriendsFlatFile {
    file: File,
}

impl Friends for FriendsFlatFile {
    fn list_friends(&self) -> Vec<Friend> {
        let buf = BufReader::new(&self.file);

        buf.lines()
            .skip(1)
            .map(|l| {
                let l = l.unwrap();
                let [last_name, first_name, birthday, email]: [String; 4] = l
                    .splitn(4, ',')
                    .map(|column| column.trim().to_string())
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap();

                let birthday = NaiveDate::parse_from_str(birthday.as_str(), "%Y/%m/%d").unwrap();
                Friend::new(last_name, first_name, email, birthday)
            })
            .collect()
    }
}

impl FriendsFlatFile {
    pub fn new(file: File) -> Self {
        Self { file }
    }
}

// Happy path
#[cfg(test)]
mod tests {
    use crate::friends_flat_file::FriendsFlatFile;
    use crate::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn have_csv_with_at_leat_one_row() {
        // Given
        let mut tmp_file = NamedTempFile::new().unwrap();
        writeln!(
            tmp_file,
            "last_name, first_name, date_of_birth, email
Doe, John, 1982/10/08, john.doe@foobar.com"
        )
        .unwrap();

        let friends = FriendsFlatFile::new(tmp_file.reopen().unwrap());

        // When
        let result = friends.list_friends();

        //Then
        assert_eq!(
            vec![Friend::new(
                "Doe".to_string(),
                "John".to_string(),
                "john.doe@foobar.com".to_string(),
                NaiveDate::from_ymd_opt(1982, 10, 08).unwrap(),
            )],
            result
        );
    }

    #[test]
    fn have_csv_with_multiple_rows() {
        // Given
        let mut tmp_file = NamedTempFile::new().unwrap();
        writeln!(
            tmp_file,
            r#"last_name, first_name, date_of_birth, email
            Doe, John, 1982/10/08, john.doe@foobar.com
            Doe, Anne, 1983/10/08, anne.doe@foobar.com"#
        )
        .unwrap();

        let friends = FriendsFlatFile::new(tmp_file.reopen().unwrap());
        // When
        let result = friends.list_friends();
        //Then
        assert_eq!(
            vec![
                Friend::new(
                    "Doe".to_string(),
                    "John".to_string(),
                    "john.doe@foobar.com".to_string(),
                    NaiveDate::from_ymd_opt(1982, 10, 08).unwrap(),
                ),
                Friend::new(
                    "Doe".to_string(),
                    "Anne".to_string(),
                    "anne.doe@foobar.com".to_string(),
                    NaiveDate::from_ymd_opt(1983, 10, 08).unwrap(),
                )
            ],
            result
        );
    }

    #[test]
    fn have_empty_csv_with_the_header() {
        // Given
        let mut tmp_file = NamedTempFile::new().unwrap();
        writeln!(tmp_file, r#"last_name, first_name, date_of_birth, email"#).unwrap();

        let friends = FriendsFlatFile::new(tmp_file.reopen().unwrap());

        // When
        let result: Vec<Friend> = friends.list_friends();

        // Then
        let expected: Vec<Friend> = vec![];
        assert_eq!(expected, result);
    }

    // have csv without header
    // have csv without header and rows
    // have csv with missing column
    // have csv with invalid birthday
}
