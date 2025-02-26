use rasm_parser::parser::ast::ASTPosition;

pub struct TextLines<'a> {
    lines: Vec<&'a str>,
}

#[derive(PartialEq, Eq)]
pub enum CharAtResult {
    Char(char),
    EndOfLine,
    Outside,
}

impl<'a> TextLines<'a> {
    pub fn new(lines: Vec<&'a str>) -> Self {
        Self { lines }
    }

    pub fn char_at_index(&self, index: &ASTPosition) -> CharAtResult {
        if let Some(line) = self.lines.get(index.row - 1) {
            if index.column == line.len() + 1 {
                CharAtResult::EndOfLine
            } else if let Some(c) = line.chars().nth(index.column - 1) {
                CharAtResult::Char(c)
            } else {
                CharAtResult::Outside
            }
        } else {
            CharAtResult::Outside
        }
    }

    pub fn move_left(&self, index: &ASTPosition) -> Option<ASTPosition> {
        let mut row = index.row as i32;
        let mut column = index.column as i32 - 1;

        if column <= 0 {
            row -= 1;
            if row <= 0 {
                return None;
            }
            column = (self.lines.get((row - 1) as usize).unwrap().len()) as i32;
        }
        Some(ASTPosition::new(row as usize, column as usize))
    }

    pub fn find_char_back_until(
        &self,
        index: &ASTPosition,
        find: &dyn Fn(char) -> bool,
    ) -> Option<ASTPosition> {
        let mut result = index.clone();
        loop {
            match self.char_at_index(&result) {
                CharAtResult::Char(c) => {
                    if find(c) {
                        return Some(result);
                    } else if c == ')' {
                        result = self.find_open_bracket(&self.move_left(&result)?)?;
                        return self.move_left(&result);
                    }
                    result = self.move_left(&result)?;
                }
                CharAtResult::EndOfLine => {
                    return None;
                }
                CharAtResult::Outside => {
                    return None;
                }
            }
        }
    }

    pub fn substr_on_same_line(
        &self,
        from: &ASTPosition,
        to: &ASTPosition, // exclusive
    ) -> Option<&'a str> {
        if from.row != to.row {
            return None;
        }
        self.lines.get(to.row - 1).and_then(|line| {
            let s = line.split_at(from.column - 1).1;
            if to.column >= from.column && s.len() >= to.column - from.column {
                Some(s.split_at(to.column - from.column).0)
            } else {
                None
            }
        })
    }

    fn find_open_bracket(&self, index: &ASTPosition) -> Option<ASTPosition> {
        let mut result = index.clone();
        let mut count = 0;
        loop {
            if let CharAtResult::Char(c) = self.char_at_index(&result) {
                if c == ')' {
                    count += 1;
                } else if c == '(' {
                    if count == 0 {
                        break;
                    } else {
                        count -= 1;
                    }
                }
                result = self.move_left(&result)?;
            } else {
                return None;
            }
        }

        Some(result)
    }
}

#[cfg(test)]
mod tests {
    use rasm_parser::parser::ast::ASTPosition;

    use crate::text_lines::TextLines;

    #[test]
    fn test_substr_on_same_line() {
        let sut = TextLines::new(vec!["Hello world"]);
        let result = sut
            .substr_on_same_line(&ASTPosition::new(1, 7), &ASTPosition::new(1, 12))
            .unwrap();

        assert_eq!("world", result);
    }
}
