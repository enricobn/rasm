use std::cell::RefCell;
use std::fs::File;
use std::io::Write;

pub static mut ENABLE_INDENT: bool = true;

thread_local! {
    pub static ENABLE_LOG: RefCell<bool> = RefCell::new(true);
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
            if $crate::debug_indent::log_enabled() {
                unsafe {
                    $crate::debug_indent::INDENT.with(|indent| {
                        let s = if !$crate::debug_indent::ENABLE_INDENT || *indent.borrow() == 0 {
                            "".into()
                        } else {
                            "|  ".repeat(*indent.borrow())
                        };
                        log::debug!("{}{}", s, & format ! ( $( $ a), * ));
                    });
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

mod tests {

    #[test]
    pub fn test() {
        indent!();
        debug_i!("debug");
        dedent!();
    }
}
