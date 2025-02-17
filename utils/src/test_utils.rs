use std::{fs, io, path::Path};

pub fn init_log() {
    let _ = env_logger::builder().is_test(true).try_init();
}

pub fn init_minimal_log() {
    let _ = env_logger::builder()
        .format_timestamp(None)
        .format_module_path(false)
        .format_target(false)
        .format_level(false)
        .is_test(true)
        .try_init();
}

///
/// indexes are 0 based and to_char is inclusive
///
pub fn read_chunk(
    path: &Path,
    from_line: usize,
    from_char: usize,
    to_line: usize,
    to_char: usize,
) -> io::Result<String> {
    let s = fs::read_to_string(path)?;

    Ok(read_chunk_string(s, from_line, from_char, to_line, to_char))
}

fn read_chunk_string(
    s: String,
    from_line: usize,
    from_char: usize,
    to_line: usize,
    to_char: usize,
) -> String {
    let lines = s.lines();

    let mut result = String::new();

    let mut i = 0;
    for line in lines.skip(from_line) {
        if i > to_line - from_line {
            break;
        }

        if i > 0 {
            result.push('\n');
        }

        let mut start = 0;
        let mut end = line.len();
        if i == 0 {
            start = from_char;
        }
        if i == to_line - from_line {
            end = to_char + 1;
        }

        let mut chunk = line.split_at(start).1;

        chunk = chunk.split_at(end - start).0;

        result.push_str(chunk);

        i += 1;
    }
    result
}

#[cfg(test)]
mod tests {
    use super::read_chunk_string;

    #[test]
    fn simple() {
        let s = read_chunk_string("hello world".to_owned(), 0, 6, 0, 10);

        assert_eq!(s, "world");
    }

    #[test]
    fn lines() {
        let s = read_chunk_string("ciao\nhello world to you".to_owned(), 1, 6, 1, 10);

        assert_eq!(s, "world");
    }

    #[test]
    fn lines2() {
        let s = read_chunk_string("ciao\nhello world to you\nciao".to_owned(), 1, 6, 1, 10);

        assert_eq!(s, "world");
    }

    #[test]
    fn lines3() {
        let s = read_chunk_string("ciao\nhello world to you\nciao".to_owned(), 1, 6, 2, 3);

        assert_eq!(s, "world to you\nciao");
    }

    #[test]
    fn lines4() {
        let s = read_chunk_string("ciao\nhello world to you\nciao".to_owned(), 0, 0, 2, 3);

        assert_eq!(s, "ciao\nhello world to you\nciao");
    }
}
