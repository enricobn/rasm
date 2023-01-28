use std::cell::RefCell;
use std::fs::File;
use std::io::Write;

pub static mut ENABLE_INDENT: bool = true;

thread_local! {
    pub static INDENT : RefCell<usize> = RefCell::new(0);
    pub static FILE : RefCell<LogFile> = RefCell::new(LogFile::new());
}

pub struct LogFile {
    file: File,
}

impl LogFile {
    pub fn new() -> Self {
        let mut file = File::create("debug.log.xml").expect("Unable to create file");

        file.write_all("<root>\n".as_bytes())
            .expect("Unable to write to log file");

        Self { file }
    }
}

impl Drop for LogFile {
    fn drop(&mut self) {
        INDENT.with(|indent| {
            let size = *indent.borrow();
            for _ in 0..size {
                self.file
                    .write_all("</children>\n".as_bytes())
                    .expect("Unable to write to log file");
            }
        });

        self.file
            .write_all("</root>\n".as_bytes())
            .expect("Unable to write to log file");
    }
}

pub fn write_to_log(message: &str) {
    let message = message
        .replace('&', "&amp;")
        .replace('"', "&quot;")
        .replace('<', "&lt;")
        .replace('>', "&gt;");
    FILE.with(|file| {
        file.borrow_mut()
            .file
            .write_all(format!("<log message=\"{message}\"/>\n").as_bytes())
            .expect("Unable to write to file");
    });
}

pub fn indent_to_log() {
    FILE.with(|file| {
        file.borrow_mut()
            .file
            .write_all("<children>\n".as_bytes())
            .expect("Unable to write to file");
    });
}

pub fn dedent_to_log() {
    FILE.with(|file| {
        file.borrow_mut()
            .file
            .write_all("</children>\n".as_bytes())
            .expect("Unable to write to file");
    });
}

#[macro_export]
macro_rules! debug_i {
    ($ ( $ a: expr), *) => {
        unsafe {
            $crate::utils::debug_indent::INDENT.with(|indent| {
                let s = if !$crate::utils::debug_indent::ENABLE_INDENT || *indent.borrow() == 0 {
                    "".into()
                } else {
                    "|  ".repeat(*indent.borrow())
                };
                debug ! ("{}{}", s, & format ! ( $( $ a), * ));
            });
        }
        $crate::utils::debug_indent::write_to_log(& format ! ( $( $ a), * ));
    };
}

#[macro_export]
macro_rules! indent {
    () => {
        unsafe {
            if $crate::utils::debug_indent::ENABLE_INDENT {
                $crate::utils::debug_indent::INDENT.with(|indent| {
                    *indent.borrow_mut() += 1;
                });
            }
        }
        $crate::utils::debug_indent::indent_to_log();
    };
}

#[macro_export]
macro_rules! dedent {
    () => {
        unsafe {
            if $crate::utils::debug_indent::ENABLE_INDENT {
                $crate::utils::debug_indent::INDENT.with(|indent| {
                    *indent.borrow_mut() -= 1;
                });
            }
        }
        $crate::utils::debug_indent::dedent_to_log();
    };
}
