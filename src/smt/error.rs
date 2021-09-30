
#[derive(Debug)]
pub enum SmtError {
    FailedToCreateTempFile,
    FailedToWriteFile(String),
    FailedToReadFile(String),
    FailedToExecuteCommand(Vec<String>),
    Unexpected(String),
}

