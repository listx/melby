use std::collections::HashMap;

// Parse the contents of a path aliases file. The keys of the resulting map are
// the paths and the values are the aliases, because that's how we use them
// back in Elixir.
#[cfg_attr(feature = "elixir_support", rustler::nif)]
pub fn parse_path_aliases(path_aliases_raw: &str) -> Result<HashMap<String, String>, String> {
    _parse_path_aliases(path_aliases_raw)
}

fn _parse_path_aliases(path_aliases_raw: &str) -> Result<HashMap<String, String>, String> {
    let mut p = Parser {
        s: path_aliases_raw,
        pos: 0,
    };
    let mut path_aliases = HashMap::new();
    // Clear leading whitespace/comments.
    p.sc();
    while p.peek().is_some() {
        let (path, alias) = p.path_alias()?;
        path_aliases.insert(path, alias);
    }
    Ok(path_aliases)
}

struct Parser<'a> {
    s: &'a str,
    // Byte offset into s.
    pos: usize,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> Option<char> {
        self.s[self.pos..].chars().next()
    }

    fn rest(&self) -> &'a str {
        &self.s[self.pos..]
    }

    fn error<T>(&self, expected: &str) -> Result<T, String> {
        let found = match self.peek() {
            Some(c) => format!("{:?}", c),
            None => "end of input".to_string(),
        };
        Err(format!(
            "offset {}: unexpected {}; expecting {}",
            self.pos, found, expected
        ))
    }

    fn take_while(&mut self, f: impl Fn(char) -> bool) -> &'a str {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if !f(c) {
                break;
            }
            self.pos += c.len_utf8();
        }
        &self.s[start..self.pos]
    }

    // Skip over any whitespace and comment lines. Comments start with "#" and
    // run up to the end of the line.
    fn sc(&mut self) {
        loop {
            if !self.take_while(is_space).is_empty() {
                continue;
            }
            if self.rest().starts_with('#') {
                self.take_while(|c| c != '\n');
                continue;
            }
            break;
        }
    }

    fn symbol(&mut self, sym: &str) -> Result<(), String> {
        if !self.rest().starts_with(sym) {
            return self.error(&format!("{:?}", sym));
        }
        self.pos += sym.len();
        self.sc();
        Ok(())
    }

    // Parse a line like
    //
    //    hash -d   kt=${HOME}/go/src/k8s.io/test-infra
    //
    // into the path and alias.
    fn path_alias(&mut self) -> Result<(String, String), String> {
        self.symbol("hash")?;
        self.symbol("-d")?;
        let alias = self.alias()?;
        if self.peek() != Some('=') {
            return self.error("'=' or alias chars");
        }
        self.pos += 1;
        let path = self.directory_path()?;
        self.sc();
        Ok((path, alias))
    }

    fn alias(&mut self) -> Result<String, String> {
        // The equivalent regex for the allowed range is "_A-Za-z0-9" for the
        // first letter, plus "-" for the rest. This just reflects what is
        // normally allowed by Zsh's "hash" builtin function. There, if we try
        // something like "hash -d -myalias=/a/very/long/path", the "-myalias"
        // gets interpreted as a flag to the "hash" function.
        let is_allowed_first_letter = |c: char| c.is_ascii_alphanumeric() || c == '_';
        match self.peek() {
            Some(c) if is_allowed_first_letter(c) => {}
            _ => return self.error("first letter of alias"),
        }
        Ok(self
            .take_while(|c| is_allowed_first_letter(c) || c == '-')
            .to_string())
    }

    // A directory path is made up of literal path segments and shell variables
    // like "${HOME}".
    fn directory_path(&mut self) -> Result<String, String> {
        let mut path = String::new();
        loop {
            if self.rest().starts_with("${") {
                path.push_str(&self.variable()?);
            } else {
                let segment = self.take_while(is_allowed_path_char);
                if segment.is_empty() {
                    break;
                }
                path.push_str(segment);
            }
        }
        if path.is_empty() {
            return self.error("directory path");
        }
        // We don't allow a "$" symbol unless it is used as part of a shell
        // variable like "${FOO}". This catches badly formed paths like
        // "/clam-chowder-$oup".
        if self.peek() == Some('$') {
            return self.error("directory chars or dollar-and-opening-brace");
        }
        Ok(path)
    }

    fn variable(&mut self) -> Result<String, String> {
        self.pos += "${".len();
        let is_allowed_first_letter = |c: char| c.is_ascii_uppercase() || c == '_';
        match self.peek() {
            Some(c) if is_allowed_first_letter(c) => self.pos += 1,
            _ => return self.error("first letter of shell variable name"),
        }
        let rest = self.take_while(|c| is_allowed_first_letter(c) || c.is_ascii_digit());
        if rest.is_empty() {
            return self.error("shell variable name chars");
        }
        let name = &self.s[self.pos - rest.len() - 1..self.pos];
        if self.peek() != Some('}') {
            return self.error("'}'");
        }
        self.pos += 1;
        Ok(format!("${{{}}}", name))
    }
}

// Like Haskell's Data.Char.isSpace.
fn is_space(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\r' | '\x0b' | '\x0c' | '\u{a0}')
        || (c > '\u{ff}' && c.is_whitespace() && c != '\u{2028}' && c != '\u{2029}')
}

// Technically speaking, a folder can have a literal "$" character in it, like
// this: "/foo/bar/\$baz", but in order to simplify our implementation we forbid
// them because they are too rare. Apart from that, we allow letters, digits,
// and punctuation. For ASCII, "punctuation" means the same characters as for
// Haskell's Data.Char.isPunctuation (so for example "+" and "~" are not
// allowed, because they are symbols). We don't treat any non-ASCII characters
// as punctuation.
fn is_allowed_path_char(c: char) -> bool {
    c != '$'
        && (c.is_alphanumeric()
            || (c.is_ascii_punctuation() && !matches!(c, '+' | '<' | '=' | '>' | '^' | '`' | '|' | '~')))
}

#[cfg(test)]
mod test {
    use super::*;

    fn parse_one(s: &str) -> Result<(String, String), String> {
        let mut p = Parser { s, pos: 0 };
        p.path_alias()
    }

    fn ok(path: &str, alias: &str) -> Result<(String, String), String> {
        Ok((path.to_string(), alias.to_string()))
    }

    fn err_at(r: Result<(String, String), String>, offset: usize) {
        match r {
            Err(e) => assert!(e.starts_with(&format!("offset {}:", offset)), "{}", e),
            Ok(x) => panic!("unexpected success: {:?}", x),
        }
    }

    #[test]
    fn test_path_alias() {
        assert_eq!(parse_one("hash -d foo=/bar"), ok("/bar", "foo"));
        assert_eq!(parse_one("hash   -d     foo=/bar   "), ok("/bar", "foo"));
        assert_eq!(
            parse_one("hash -d   kk=${HOME}/go/src/k8s.io/kubernetes"),
            ok("${HOME}/go/src/k8s.io/kubernetes", "kk")
        );
        assert_eq!(
            parse_one("hash -d   x=${HOME}/foo/${SUBDIR}/baz"),
            ok("${HOME}/foo/${SUBDIR}/baz", "x")
        );
        assert_eq!(
            parse_one("hash -d   x=${HOME}/hello # Comment text"),
            ok("${HOME}/hello", "x")
        );
    }

    #[test]
    fn test_path_alias_errors() {
        // The equal sign has a space to the left.
        err_at(parse_one("hash   -d     foo =/bar   "), 17);
        // The equal sign has a space to the right.
        err_at(parse_one("hash   -d     foo= /bar   "), 18);
        // There is an aberrant dollar sign in the path.
        err_at(parse_one("hash -d x=/clam-chowder-$oup\n"), 24);
        // Shell variables must be in uppercase, and have at least 2 letters.
        err_at(parse_one("hash -d x=${home}"), 12);
        err_at(parse_one("hash -d x=${H}"), 13);
        // Aliases cannot start with a dash.
        err_at(parse_one("hash -d -x=/foo"), 8);
    }

    #[test]
    fn test_path_aliases() {
        let input = "
# Comment at beginning of file.

hash -d x=/foo # Trailing comment.
# Commented out declaration.
# hash -d x=/foo

hash -d kk=${HOME}/go/src/k8s.io/kubernetes

# Indented entry.
  hash    -d   y=/hello
# Comment at end of file.
";
        let want: HashMap<String, String> = [
            ("/foo", "x"),
            ("${HOME}/go/src/k8s.io/kubernetes", "kk"),
            ("/hello", "y"),
        ]
        .iter()
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect();
        assert_eq!(_parse_path_aliases(input), Ok(want));
        assert_eq!(_parse_path_aliases(""), Ok(HashMap::new()));
        assert!(_parse_path_aliases("hash -d x=/foo\nbogus").is_err());
    }
}
