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
    pub at_line: usize,
    pub at_file: usize,
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

    pub fn as_string(&self, src: Vec<String>, filenames: Vec<String>) -> String {
        let lines = {
            let mut a: Vec<Vec<&str>> = Vec::new();
            for e in src.iter() {
                a.push((*e).lines().collect())
            }
            a
        };
        let mut tmp = String::new();

        use std::fmt::Write;
        for el in self.errors.iter() {
            writeln!(&mut tmp, "{}: {}:", el.lvl, el.err).unwrap();
            writeln!(&mut tmp, "    \x1b[0;33m--> \x1b[0;4m{}\x1b[0m", filenames[el.at_file]).unwrap();
            writeln!(&mut tmp, "{}{} |  {}\n",
                " ".repeat(4 - format!("{}", el.at_line).len().min(0)),
                el.at_line.min(lines[el.at_file].len()-1) + 1,
                lines[el.at_file][el.at_line.min(lines[el.at_file].len()-1)].trim(),
            ).unwrap();
        }
        tmp.pop();
        tmp.pop();

        tmp
    }
}

impl Error {
    pub fn new(err: ErrorKind, lvl: ErrorLevel, at_line: usize, at_file: usize) -> Self {
        Error { err, lvl, at_line, at_file }
    }
}