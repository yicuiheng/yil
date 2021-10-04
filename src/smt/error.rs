#[derive(Debug, PartialEq, Eq)]
pub enum SmtError {
    FailedToCreateTempFile,
    FailedToWriteFile(String),
    FailedToExecuteCommand(Vec<String>),
    Unexpected(String),
}
