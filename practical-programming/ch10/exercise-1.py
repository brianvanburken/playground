def backup(file_name: str) -> None:
    """
    Backups a file by creating a clone with the extension .bak appended.
    """

    with open(file_name) as original_file, \
         open(file_name + '.bak', 'w') as backup_file:
        backup_file.write(original_file.read())
        original_file.close()
        backup_file.close()


if __name__ == '__main__':
    file_name = input('Fill in path to file to back up:')
    backup(file_name)
