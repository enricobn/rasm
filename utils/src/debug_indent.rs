use std::cell::RefCell;
use std::fs::File;
use std::io::Write;

use quick_xml::Writer;
use quick_xml::events::{BytesEnd, BytesStart, Event};

pub static mut ENABLE_INDENT: bool = true;
pub static mut ENABLE_LOG_STDOUT: bool = false;

thread_local! {
    pub static ENABLE_LOG: RefCell<bool> = RefCell::new(true);
    pub static INDENT : RefCell<usize> = RefCell::new(0);
    pub static LOG_FILE : RefCell<LogFile> = RefCell::new(LogFile::new(File::create("debug.log.xml").expect("Unable to create file")));
}

pub struct LogFile {
    writer: Writer<File>,
}

impl LogFile {
    fn new(file: File) -> Self {
        let mut writer = Writer::new(file);

        writer
            .write_event(Event::Start(BytesStart::new("root")))
            .expect("Unable to write to log file");
        writer
            .get_mut()
            .write_all(b"\n")
            .expect("Unable to write to log file");

        Self { writer }
    }

    pub fn indent(&mut self) {
        self.writer
            .write_event(Event::Start(BytesStart::new("children")))
            .expect("Unable to write to file");
        self.writer
            .get_mut()
            .write_all(b"\n")
            .expect("Unable to write to file");
    }

    pub fn dedent(&mut self) {
        self.writer
            .write_event(Event::End(BytesEnd::new("children")))
            .expect("Unable to write to file");
        self.writer
            .get_mut()
            .write_all(b"\n")
            .expect("Unable to write to file");
    }

    pub fn write(&mut self, message: &str) {
        let sanitized = sanitize_for_xml(message);

        let mut elem = BytesStart::new("log");
        elem.push_attribute(("message", sanitized.as_str()));
        self.writer
            .write_event(Event::Empty(elem))
            .expect("Unable to write to file");
        self.writer
            .get_mut()
            .write_all(b"\n")
            .expect("Unable to write to file");
    }

    fn flush(&mut self) {
        INDENT.with(|indent| {
            let size = *indent.borrow();
            for _ in 0..size {
                self.writer
                    .write_event(Event::End(BytesEnd::new("children")))
                    .expect("Unable to write to log file");
                self.writer
                    .get_mut()
                    .write_all(b"\n")
                    .expect("Unable to write to log file");
            }
        });

        self.writer
            .write_event(Event::End(BytesEnd::new("root")))
            .expect("Unable to write to log file");
        self.writer
            .get_mut()
            .write_all(b"\n")
            .expect("Unable to write to log file");
        // Ensure all data is flushed to disk.
        self.writer.get_mut().flush().ok();
        println!("Debug log written to debug.log.xml");
    }
}

impl Drop for LogFile {
    fn drop(&mut self) {
        self.flush();
    }
}

fn is_valid_xml_char(c: char) -> bool {
    matches!(c, '\u{09}' | '\u{0A}' | '\u{0D}')
        || ('\u{20}'..='\u{D7FF}').contains(&c)
        || ('\u{E000}'..='\u{FFFD}').contains(&c)
        || ('\u{10000}'..='\u{10FFFF}').contains(&c)
}

fn sanitize_for_xml(s: &str) -> String {
    if s.chars().all(is_valid_xml_char) {
        return s.to_string();
    }
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        // Strip ANSI CSI escape sequences (e.g. "\x1b[31m", "\x1b[0m") entirely
        // instead of leaving "�[31m" behind. This keeps the log readable.
        if c == '\x1b' {
            if chars.peek() == Some(&'[') {
                chars.next(); // consume '['
                // consume until terminating 'm' or any final byte '@'..='~'
                for next in chars.by_ref() {
                    if next == 'm' {
                        break;
                    }
                    if ('@'..='~').contains(&next) {
                        break;
                    }
                }
                continue;
            } else {
                out.push('\u{FFFD}');
                continue;
            }
        }
        if is_valid_xml_char(c) {
            out.push(c);
        } else {
            // Use `�` (U+FFFD) to make the substitution visible.
            out.push('\u{FFFD}');
        }
    }
    out
}

pub fn write_to_log(message: &str) {
    LOG_FILE.with(|file| {
        file.borrow_mut().write(message);
    });
}

pub fn indent_to_log() {
    LOG_FILE.with(|file| {
        file.borrow_mut().indent();
    });
}

pub fn dedent_to_log() {
    LOG_FILE.with(|file| {
        let mut log_file = file.borrow_mut();
        log_file
            .writer
            .write_event(Event::End(BytesEnd::new("children")))
            .expect("Unable to write to file");
        log_file
            .writer
            .get_mut()
            .write_all(b"\n")
            .expect("Unable to write to file");
    });
}

#[macro_export]
macro_rules! debug_i {
        ($ ( $ a: expr), *) => {
            if $crate::debug_indent::log_enabled() {
                unsafe {
                    if $crate::debug_indent::ENABLE_LOG_STDOUT {
                        $crate::debug_indent::INDENT.with(|indent| {
                            let s = if !$crate::debug_indent::ENABLE_INDENT || *indent.borrow() == 0 {
                                "".into()
                            } else {
                                "|  ".repeat(*indent.borrow())
                            };
                            log::debug!("{}{}", s, & format ! ( $( $ a), * ));
                        });
                    }
                }
                if log::log_enabled!(log::Level::Debug) {
                    $crate::debug_indent::write_to_log(& format ! ( $( $ a), * ));
                }
            }
        };

}

#[macro_export]
macro_rules! indent {
    () => {
        if $crate::debug_indent::log_enabled() {
            unsafe {
                if $crate::debug_indent::ENABLE_INDENT {
                    $crate::debug_indent::INDENT.with(|indent| {
                        *indent.borrow_mut() += 1;
                    });
                }
            }
            if log::log_enabled!(log::Level::Debug) {
                $crate::debug_indent::indent_to_log();
            }
        }
    };
}

#[macro_export]
macro_rules! dedent {
    () => {
        if $crate::debug_indent::log_enabled() {
            unsafe {
                if $crate::debug_indent::ENABLE_INDENT {
                    $crate::debug_indent::INDENT.with(|indent| {
                        *indent.borrow_mut() -= 1;
                    });
                }
            }
            if log::log_enabled!(log::Level::Debug) {
                $crate::debug_indent::dedent_to_log();
            }
        }
    };
}

#[macro_export]
macro_rules! reset_indent {
    () => {
        unsafe {
            if $crate::debug_indent::ENABLE_INDENT {
                $crate::debug_indent::INDENT.with(|indent| {
                    if log::log_enabled!(log::Level::Debug) {
                        for i in 0..*indent.borrow() {
                            $crate::debug_indent::dedent_to_log();
                        }
                    }
                    *indent.borrow_mut() = 0;
                });
            }
        }
    };
}

pub fn enable_log(enable: bool) {
    ENABLE_LOG.with(|enable_log| {
        *enable_log.borrow_mut() = enable;
    });
}

pub fn log_enabled() -> bool {
    ENABLE_LOG.with(|enable_log| *enable_log.borrow())
}

#[cfg(test)]
mod tests {
    use std::fs::File;

    use tempdir::TempDir;

    use crate::debug_indent::LogFile;

    #[test]
    pub fn test() {
        indent!();
        debug_i!("debug");
        dedent!();
    }

    #[test]
    pub fn test_log_file_escape_colors() {
        test_log(
            |log_file| {
                log_file.write("\x1b[31mtest\x1b[0m");
            },
            "<root>\n<log message=\"test\"/>\n</root>\n",
        );
    }

    #[test]
    pub fn test_log_file_indent() {
        test_log(
            |log_file| {
                log_file.write("parent");
                log_file.indent();
                log_file.write("child");
                log_file.dedent();
            },
            "<root>\n<log message=\"parent\"/>\n<children>\n<log message=\"child\"/>\n</children>\n</root>\n",
        );
    }

    #[test]
    pub fn test_log_file_emoj() {
        test_log(
            |log_file| {
                log_file.write("🤣");
            },
            "<root>\n<log message=\"🤣\"/>\n</root>\n",
        );
    }

    #[test]
    pub fn test_log_file_escape_chars() {
        test_log(
            |log_file| {
                log_file.write("<");
            },
            "<root>\n<log message=\"&lt;\"/>\n</root>\n",
        );
    }

    fn test_log(f: impl Fn(&mut LogFile), expected: &str) {
        let out_folder = TempDir::new("rasm_indent_test").unwrap().into_path();
        let out_file = out_folder.as_path().join("out.xml");
        let file = File::create(&out_file).unwrap();
        let mut log_file = LogFile::new(file);
        f(&mut log_file);
        drop(log_file);
        let file = std::fs::read_to_string(out_file).unwrap();
        assert_eq!(file, expected);
    }
}
