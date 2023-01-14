use self::error::{ErrorKind, ErrorLevel};

pub mod error;

#[derive(Debug)]
pub struct ErrorHandler {
    pub errors: Vec<Error>
}

#[derive(Debug)]
pub struct Error {
    pub err: error::ErrorKind,
    pub lvl: error::ErrorLevel,
    pub at_line: usize
}


impl ErrorHandler {
    pub fn new() -> Self {
        Self {
            errors: Vec::new()
        }
    }

    pub fn add_error(&mut self, error: Error) {
        self.errors.push(error)
    }

    pub fn as_string(&self, src: String) -> String {
        let lines = src.lines().collect::<Vec<&str>>();
        let mut tmp = String::new();

        use std::fmt::Write;
        for el in self.errors.iter() {
            writeln!(&mut tmp, "{}: {}:", el.lvl, el.err).unwrap();
            writeln!(&mut tmp, "{}{} |  {}", " ".repeat(4 - format!("{}", el.at_line).len().min(0)), el.at_line + 1, lines[el.at_line].trim()).unwrap();
        }

        tmp
    }
}

impl Error {
    pub fn new(err: ErrorKind, lvl: ErrorLevel, at_line: usize) -> Self {
        Error { err, lvl, at_line }
    }
}