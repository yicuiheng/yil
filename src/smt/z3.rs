use super::error::SmtError;

pub fn check_unsat(query: String) -> Result<bool, SmtError> {
    use std::io::Write;

    let mut temp = tempfile::Builder::new()
        .suffix(".smtlib2")
        .tempfile()
        .map_err(|_| SmtError::FailedToCreateTempFile)?;

    let z3_input_filename = temp.path().to_str().unwrap().to_string();

    write!(temp, "{}", query)
        .map_err(|_| SmtError::FailedToWriteFile(z3_input_filename.clone()))?;

    let output = std::process::Command::new("z3")
        .arg(&z3_input_filename)
        .output()
        .map_err(|_| SmtError::FailedToExecuteCommand(vec!["z3".to_string(), z3_input_filename]))?;

    if output.status.code().unwrap() != 0 {
        let err_msg = format!(
            r#"
[UNEXPECTED ERROR] z3 cannot recognized the query..
please contact me, yicuiheng <yicuiheng@gmail.com>
z3 query:
```
{}
```
error:
```
{}
```"#,
            query,
            std::str::from_utf8(output.stderr.as_slice()).unwrap()
        );
        return Err(SmtError::Unexpected(err_msg));
    }

    if !output.stderr.is_empty() {
        let warning_msg = format!(
            r#"
[UNEXPECTED WARNING] warning occured in z3..
please contact me, yicuiheng <yicuiheng@gmail.com>
z3 query:
```
{}
```
warning:
```

{}
```"#,
            query,
            std::str::from_utf8(output.stderr.as_slice()).unwrap()
        );
        return Err(SmtError::Unexpected(warning_msg));
    }

    match std::str::from_utf8(output.stdout.as_slice()).unwrap() {
        "sat\n" => Ok(false),
        "unsat\n" => Ok(true),
        _ => {
            let msg = format!(
                r#"
[UNEXPECTED ERROR] z3 cannot recognized the query generated from yil type system..
please contact me, yicuiheng <yicuiheng@gmail.com>
z3 query:
```
{}
```
output:
```
{}
```
warning:
```
{}
```"#,
                query,
                std::str::from_utf8(output.stdout.as_slice()).unwrap(),
                std::str::from_utf8(output.stderr.as_slice()).unwrap()
            );
            Err(SmtError::Unexpected(msg))
        }
    }
}
