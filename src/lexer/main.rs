
    pub fn next_token(&mut self) -> Result<Token,ParseError> {
        loop {
            self.skip_white();

            if self.is_eof() {
                return self.eof()

            } else if self.is_comment_start() {
                self.skip_comment()

            } else if self.is_multiline_comment_start() {
                self.skip_multiline_comment()

            } else if self.is_identifier_start() {
                return self.read_identifier()

            } else if self.is_digit() {
                return self.read_number()

            } else {
                let ch = self.looks[ 0 ];
                return self.build_error(format!("unknown character {}",ch.value), ch.position)
            }

        }
    }

    fn eof() -> Result<Token,ParseError> {

    }

    fn skip_white(&mut self) -> Result<Token,ParseError> {
        while self.is_whitespace() {
            self.read_char();
        }
    }

    fn skip_comment(&mut self) -> Result<Token,ParseError> {
        self.read_char();
        self.read_char();

        while !self.is_comment_end() {
            self.read_char();
        }

        self.read_char();
    }

    fn skip_multiline_comment(&mut self) -> Result<Token,ParseError> {
        self.read_char();
        self.read_char();

        while !self.is_multiline_comment_end() {
            self.read_char();
        }

        self.read_char();
        self.read_char();
    }

    fn read_number(&mut self) -> Result<Token,ParseError> {
        let mut token = self.build_token();

        while self.is_digit() {
            let val = self.read_char().unwrap().value;
            token.value.push(val);
        }

        Ok(token)
    }

    fn read_identifier(&mut self) -> Result<Token,ParseError> {
        let mut token = self.build_token();

        while self.is_identifier_char() {
            let val = self.read_char().unwrap().value;
            token.value.push(val);
        }

        Ok(token)
    }

    fn read_char(&mut self) -> Result<Character,ParseError> {
        self.read_char_into_buffer();
        self.looks.remove(0)
    }

    fn read_char_into_buffer(&mut self) -> Option<ParseError> {
        match self.file.read_char() {
            Ok(val) => {
                let ch = self.build_char(val);
                self.looks.push(ch)
            },
            _ => {}
        }
    }

    fn build_token(&self) -> Token {
        let pos = self.looks[0].position;

        Token { value: "".to_string(), position: pos }
    }

    fn build_error( &self, msg : String, pos : Position ) -> ParseError {
        ParseError { filename: self.filename, message: msg, position: pos }
    }

    fn build_char(&mut self, value: char) -> Character {
        let mut pos;

        if self.looks.is_empty() {
            pos = Position { line: 1, column: 1 };
        } else {
            let last = &self.looks[self.looks.len()-1];
            pos = last.position;

            if last.value == '\n' {
                pos.line += 1;
                pos.column = 1;
            } else {
                pos.column += 1;
            }
        };

        Character { value: value, position: pos }
    }

    fn is_comment_start(&self) -> bool {
        self.nth(0).map_or(false, |c| c == '/') && self.nth(1).map_or(false, |c| c == '/')
    }

    fn is_comment_end(&self) -> bool {
        self.nth(0).map_or(false, |c| c == '\n')
    }

    fn is_multiline_comment_start(&self) -> bool {
        self.nth(0).map_or(false, |c| c == '/') && self.nth(1).map_or(false, |c| c == '*')
    }

    fn is_multiline_comment_end(&self) -> bool {
        self.nth(0).map_or(false, |c| c == '*') && self.nth(1).map_or(false, |c| c == '/')
    }

    fn is_digit(&self) -> bool {
        self.look().map_or(false, |ch| ch >= '0' && ch <= '9')
    }

    fn is_identifier_start(&self) -> bool {
        self.look().map_or(false, |ch| {
            ( ch >= 'a' && ch <= 'z' ) ||
            ( ch >= 'A' && ch <= 'Z' ) || ch == '_'
        } )
    }

    fn is_identifier_char(&self) -> bool {
        self.look().map_or(false, |ch| {
            ( ch >= 'a' && ch <= 'z' ) ||
            ( ch >= 'A' && ch <= 'Z' ) || ch == '_' ||
            ( ch >= '0' && ch <= '9' )
        } )
    }

    fn is_whitespace(&self) -> bool {
        self.nth(0).map_or(false, |c| c.is_whitespace())
    }

    fn is_eof(&self) -> bool {
        self.look().is_none()
    }

    fn look(&self) -> Option<char> {
        self.nth(0)
    }

    fn nth(&self, ind : uint ) -> Option<char> {
        if ind < self.looks.len() {
            Some(self.looks[ind].value)
        } else {
            None
        }
    }
}
