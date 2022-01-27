use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use std::ops::Range;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum ErrorType {
    Lexer,
    Parser,
    Scope,
}

#[derive(Debug)]
pub struct Error {
    error_type: ErrorType,
    message: String,
    range: Range<usize>,
    source_file: String,
}

impl Error {
    pub fn new(
        error_type: ErrorType,
        message: String,
        range: Range<usize>,
        source_file: String,
    ) -> Self {
        Self {
            error_type,
            message,
            range,
            source_file,
        }
    }

    pub fn print(&self) -> std::io::Result<()> {
        let source_code = std::fs::read_to_string(&self.source_file)?;
        let mut color_generator = ColorGenerator::new();

        Report::build(ReportKind::Error, &self.source_file, 10)
            .with_message(format!("{:?} error encountered", self.error_type))
            .with_label(
                Label::new((&self.source_file, self.range.clone()))
                    .with_message(&self.message)
                    .with_color(color_generator.next()),
            )
            .finish()
            .print((&self.source_file, Source::from(source_code)))
            .unwrap();

        Ok(())
    }
}
