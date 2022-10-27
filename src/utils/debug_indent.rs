use std::cell::RefCell;

pub static mut ENABLE_INDENT: bool = true;

thread_local! {
    pub static INDENT : RefCell<usize> = RefCell::new(0);
}

#[macro_export] macro_rules! debug_i {
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
    };
}

#[macro_export] macro_rules! indent {
    () => {
        unsafe {
            if crate::utils::debug_indent::ENABLE_INDENT {
                crate::utils::debug_indent::INDENT.with(|indent| {
                    *indent.borrow_mut() += 1;
                });
            }
        }
    };
}

#[macro_export] macro_rules! dedent {
    () => {
        unsafe {
            if crate::utils::debug_indent::ENABLE_INDENT {
                crate::utils::debug_indent::INDENT.with(|indent| {
                    *indent.borrow_mut() -= 1;
                });
            }
        }
    };
}