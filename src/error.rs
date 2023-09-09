use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};

use crate::lexer::SourceRange;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorType {
    Lexer,
    Parser,
    Scope,
    TypeCheck,
    Postprocess,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    error_type: ErrorType,
    pub message: String,
    range: Option<SourceRange>,
}

impl Error {
    pub fn new(error_type: ErrorType, message: String) -> Self {
        Self {
            error_type,
            message,
            range: None,
        }
    }

    pub fn new_with_range(error_type: ErrorType, message: String, range: SourceRange) -> Self {
        Self {
            error_type,
            message,
            range: Some(range),
        }
    }

    pub fn new_complete(error_type: ErrorType, message: String, range: SourceRange) -> Self {
        Self {
            error_type,
            message,
            range: Some(range),
        }
    }

    pub fn with_range(self, range: SourceRange) -> Self {
        Self {
            error_type: self.error_type,
            message: self.message,
            range: Some(range),
        }
    }

    #[cfg(not(tarpaulin_include))]
    pub fn print(&self, source_file: &str) -> std::io::Result<()> {
        let source_code = std::fs::read_to_string(source_file)?;
        let mut color_generator = ColorGenerator::new();

        let mut builder = Report::build(ReportKind::Error, &source_file, 10);

        if let Some(range) = self.range {
            builder = builder
                .with_message(format!("{:?} error encountered", self.error_type))
                .with_label(
                    Label::new((&source_file, range.into()))
                        .with_message(&self.message)
                        .with_color(color_generator.next()),
                );
        } else {
            builder = builder.with_message(format!(
                "{:?} error encountered: {}",
                self.error_type, self.message
            ));
        }

        builder
            .finish()
            .print((&source_file, Source::from(source_code)))
            .unwrap();

        Ok(())
    }
}
