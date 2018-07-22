use std::io::{self, Write, BufRead, BufReader, BufWriter};
use std::fs::{File};

type Lines = Option<(usize, usize)>;

enum Mode {
    Command,
    Input,
}

enum PrintFlag {
    Print,
    Enumerate,
    List,
}

struct Line {
    mark: bool,
    mark_char: Option<char>,
    text: String,
}

impl Line {
    fn new(s: String) -> Self {
        Line {
            mark: false,
            mark_char: None,
            text: s,
        }
    }
}

struct State {
    mode: Mode,
    default_filename: Option<String>,
    lines: Vec<Line>,
    current_line: usize,
    modified: bool,
    last_command: Option<char>,
    prompt: Option<String>,
    print_prompt: bool,
    last_pattern: Option<Vec<Pat>>,
    cut_buffer: Option<Vec<String>>,
    input_lines: Vec<String>,
    input_line: Vec<char>,
    input_p: usize,
    exec_global: bool,
}

impl State {
    fn new() -> Self {
        State{
            mode: Mode::Command,
            default_filename: None,
            lines: Vec::new(),
            current_line: 0,
            modified: false,
            last_command: None,
            prompt: Some("*".into()),
            print_prompt: false,
            last_pattern: None,
            cut_buffer: None,
            input_lines: Vec::new(),
            input_line: Vec::new(),
            input_p: 0,
            exec_global: false,
        }
    }

    fn mark_line(&mut self, line: usize, mark: char) {
        for (i, l) in self.lines.iter_mut().enumerate() {
            if i == line - 1 {
                l.mark_char = Some(mark);
            } else {
                l.mark_char = None;
            }
        }
    }

    fn unmark(&mut self) {
        for l in &mut self.lines {
            l.mark = false;
        }
    }

    fn set_lines(&mut self, l: Vec<String>) {
        self.lines.clear();
        for s in l {
            self.lines.push(Line::new(s.into()));
        }
        self.current_line = self.last_line();
    }

    fn insert_lines(&mut self, after_line: usize, l: Vec<String>) {
        for (i, s) in l.iter().enumerate() {
            self.lines.insert(after_line + i, Line::new(s.clone()));
        }
        self.modified = true;
        self.current_line = after_line + l.len();
    }

    fn append(&mut self, insertion_line: usize, lines: Vec<String>) {
        for (i, l) in lines.iter().enumerate() {
            self.lines.insert(insertion_line + i, Line::new(l.clone()));
        }
        self.modified = true;
    }

    fn delete(&mut self, from_line: usize, to_line: usize) {
        let _removed: Vec<Line> = self.lines.drain(from_line - 1..=to_line - 1).collect();
        self.modified = true;
    }

    fn take(&mut self, from_line: usize, to_line: usize) -> Vec<String> {
        self.modified = true;
        self.lines.drain(from_line - 1..=to_line - 1).map(| l | l.text).collect()
    }

    fn copy(&mut self, from_line: usize, to_line: usize) -> Vec<String> {
        self.lines.as_slice()[from_line - 1..=to_line - 1].iter().map(| l | l.text.clone()).collect()
    }

    fn yank(&mut self, from_line: usize, to_line: usize) {
        self.cut_buffer = Some(self.lines.as_slice()[from_line - 1..=to_line - 1].iter()
                               .map(| l | l.text.clone()).collect());
    }

    fn copy_cut_buffer(&self) -> Option<Vec<String>> {
        match &self.cut_buffer {
            None => None,
            Some(v) => Some(v.clone()),
        }
    }

    fn last_line(&self) -> usize {
        self.lines.len()
    }

    fn next_line(&mut self) {
        self.current_line += 1;
        if self.current_line > self.last_line() {
            self.current_line = 1;
        }
    }

    fn read_line(&mut self) -> io::Result<Option<String>> {
        if self.input_lines.is_empty() && !self.exec_global {
            let stdin = io::stdin();
            let mut stdout = io::stdout();

            prompt(self, &mut stdout)?;

            let mut buf = String::new();
            let mut cnt = stdin.read_line(&mut buf)?;
            if cnt > 0 {
                while buf.ends_with("\\\n") {
                    let _ = buf.pop();
                    let _ = buf.pop();
                    buf.push('\n');
                    cnt = stdin.read_line(&mut buf)?;
                    if cnt == 0 {
                        break;
                    }
                }
            } else {
                return Ok(None);
            }
            return Ok(Some(buf));
        } else {
            if self.input_lines.is_empty() {
                Ok(None)
            } else {
                let buf = self.input_lines.remove(0);
                Ok(Some(buf))
            }
        }
    }

    fn drop_char(&mut self) -> io::Result<()> {
        self.input_p += 1;
        Ok(())
    }

    fn eol(&self) -> bool {
        if self.input_p < self.input_line.len() {
            self.input_line[self.input_p] == '\n'
        } else {
            true
        }
    }
    // Return the current input character, or None if at the end of
    // input.
    fn current_char(&mut self) -> io::Result<Option<char>> {
        if self.input_p >= self.input_line.len() {
            if let Some(s) = self.read_line()? {
                self.input_line = s.chars().collect();
                self.input_p = 0;
            } else {
                return Ok(None);
            }
        }
        let c = self.input_line[self.input_p];
        Ok(Some(c))
    }

    // Skip all horizontal whitespace (if any).
    fn skip_ws(&mut self) -> io::Result<()> {
        while self.input_p < self.input_line.len() {
            match self.input_line[self.input_p] {
                ' ' => {},
                '\t' => {},
                _ => break,
            }
            self.skip_char()?;
        }
        Ok(())
    }

    // Skip all characters up to and including the newline '\n'
    // character.
    fn skip_to_nl(&mut self) -> io::Result<()> {
        while let Some(c) = self.current_char()? {
            match c {
                '\n' => {
                    self.skip_char()?;
                    break;
                },
                _ => {
                    self.skip_char()?;
                },
            }
        }
        Ok(())
    }

    // Skip the current input char (if any).
    fn skip_char(&mut self) -> io::Result<()> {
        if self.input_p >= self.input_line.len() {
            if let Some(s) = self.read_line()? {
                self.input_line = s.chars().collect();
                self.input_p = 0;
            } else {
                return Ok(());
            }
        }
        self.input_p += 1;
        Ok(())

    }

    // Return all characters until the end of the current input line.
    fn take_to_end(&mut self) -> io::Result<String> {
        let mut buf = String::new();
        while let Some(c) = self.current_char()? {
            buf.push(c);
            self.skip_char()?;
        }
        Ok(buf)
    }

    fn append_input_lines(&mut self, l: Vec<String>) {
        self.input_lines.extend(l);
    }
}

// Construct an IO error of the given kind and error message.
fn err<R>(kind: io::ErrorKind, msg: &str) -> io::Result<R> {
    Err(io::Error::new(kind, msg.to_string()))
}

// Construct an "invalid line address" error.
fn address_err<R>() -> io::Result<R> {
    err(io::ErrorKind::InvalidData, "invalid line address")
}

// Construct a "line address missing" error.
fn missing_address_err<R>() -> io::Result<R> {
    err(io::ErrorKind::InvalidData, "line address missing")
}

// Construct a "pattern missing" error.
fn missing_pattern_err<R>() -> io::Result<R> {
    err(io::ErrorKind::InvalidData, "pattern missing")
}

// Construct a "argument missing" error.
fn missing_argument_err<R>() -> io::Result<R> {
    err(io::ErrorKind::InvalidData, "argument missing")
}

// Construct a "invalid pattern" error.
fn invalid_pattern_err<R>() -> io::Result<R> {
    err(io::ErrorKind::InvalidData, "invalid pattern")
}

// Return an error value if the given input contains any remaining
// non-whitespace characters.  Will also ignore newlines and carriage
// returns.
fn any_arg_err(state: &mut State) -> io::Result<()> {
    state.skip_ws()?;
    match state.current_char()? {
        None => Ok(()),
        Some('\n') => {
            state.drop_char()?;
            Ok(())
        },
        Some(_) => {
            state.skip_to_nl()?;
            err(io::ErrorKind::InvalidData, "no parameters allowed")
        },
    }
}

// Return an error value if the given value is 0.
fn zero_address_err(val: usize) -> io::Result<()> {
    if val == 0 {
        err(io::ErrorKind::InvalidData, "address may not be zero")
    } else {
        Ok(())
    }
}

// Return an error value of the given `l` contains addresses.
fn any_address_err(l: Lines) -> io::Result<()> {
    match l {
        None =>
            Ok(()),
        Some(_) =>
            err(io::ErrorKind::InvalidData, "no address allowed")
    }

}

// Write a prompt to stdout (without a newline) and flush it to the
// terminal.
fn prompt<W: Write>(state: &State, stdout: &mut W) -> io::Result<()> {
    match (&state.mode, state.print_prompt) {
        (Mode::Command, true) =>
            match &state.prompt {
                None => {
                    return err(io::ErrorKind::Other, "no prompt set");
                },
                Some(p) => {
                    print!("{}", p);
                    stdout.flush()?
                },
            },
        _ => {},
    }
    Ok(())
}

// Convert a character in the range '0'..'9' (inclusive) to its
// numeric counterpart in the range 0..9.
fn atoi(c: char) -> usize {
    c as usize - b'0' as usize
}

// Return the two numbers in `l` as a two-tuple, if present, otherwise
// returns the two numeric arguments in a tuple.
fn default_lines(l: Lines, line1: usize, line2: usize) -> (usize, usize) {
    match l {
        None => (line1, line2),
        Some((l1, l2)) => (l1, l2),
    }
}

// Check the input for an print command (`p`, `n` or `l`) immediately
// following a command (no leading whitespace allowed).  If found,
// skip the command character, convert it into a print flag and return
// it. If not found, return None.  The print flag can then be passed
// to the `do_print` function in order to conditionally print a range
// of lines.
fn print_flag(state: &mut State) -> io::Result<Option<PrintFlag>>
{
    if !state.eol() {
        if let Some(c) = state.current_char()? {
            match c {
                'p' => { state.skip_char()?; Ok(Some(PrintFlag::Print)) },
                'n' => { state.skip_char()?; Ok(Some(PrintFlag::Enumerate)) },
                'l' => { state.skip_char()?; Ok(Some(PrintFlag::List)) },
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    } else {
        Ok(None)
    }
}

// Parse a decimal integer from the input. Starts with the character
// `c`, which must be in the range '0'..'9' (inclusive).  Will stop as
// soon as a non-numeric character is found (or a the end of input).
// Returns the result wrapped in a `io::Result` for convenience.
fn get_num(state: &mut State, c: char) -> io::Result<usize> {
    let mut acc = atoi(c);
    state.skip_char()?;
    while let Some(c) = state.current_char()? {
        match c {
            '0'..='9' => {
                acc = acc * 10 + atoi(c);
                state.skip_char()?;
            },
            _ => {
                break;
            }
        }
    }
    Ok(acc)
}

// Calculate a new line number based on the base term `base` and the
// increment `amount`, which is added if `sign` is `+` and subtracted
// if `sign` is `-`.  An error value is returned if the resulting line
// number is out of bounds.  Note that this might return a line number
// of 0.  Returns the result wrapped in a `io::Result` for
// convenience.
fn get_rel_line(state: &State, sign: char, base: usize, amount: usize) -> io::Result<Option<usize>> {
    if sign == '-' {
        if base >= amount {
            Ok(Some(base - amount))
        } else {
            address_err()
        }
    } else {
        if base + amount <= state.last_line() {
            Ok(Some(base + amount))
        } else {
            address_err()
        }
    }
}

// Increment the line number `l`, wrapping around to the first line if
// necessary.
fn inc_mod_line(l: usize, last: usize) -> usize {
    if l < last {
        l + 1
    } else {
        1
    }
}

// Decrement the line number `l`, wrapping around to the last line if
// necessary.
fn dec_mod_line(l: usize, last: usize) -> usize {
    if l > 1 {
        l - 1
    } else {
        last
    }
}

// Get the line address matching the pattern at the current input
// position, which starts with character `scan_char` (either `/` or
// `?`).
fn get_scan_pattern(state: &mut State, scan_char: char) -> io::Result<usize>
{
    let pat = get_pattern(state)?;
    let search_pat =
        if pat.len() == 0 {
            match &state.last_pattern {
                None => return err(io::ErrorKind::InvalidData, "no previous pattern"),
                Some(p) => p.clone(),
            }
        } else {
            pat
        };

    // Start on the line following or preceding the current line,
    // depending on scan direction.
    let mut l =
        if scan_char == '/' {
            inc_mod_line(state.current_line, state.last_line())
        } else {
            dec_mod_line(state.current_line, state.last_line())
        };
    let mut res = None;
    loop {
        let mtch = match_pattern(&search_pat, &state.lines[l - 1].text);

        // Stop scanning when we found a match.
        if let Some(_pos) = mtch {
            res = Some(l);
            break;
        }

        // Stop when we wrapped back to the start line.
        if l == state.current_line {
            break;
        }

        // Go to next or previous line, again depending on scan
        // direction.
        l =
            if scan_char == '/' {
                inc_mod_line(l, state.last_line())
            } else {
                dec_mod_line(l, state.last_line())
            };
    }
    match res {
        None => return err(io::ErrorKind::InvalidData, "no match"),
        Some(n) => {
            state.last_pattern = Some(search_pat);
            Ok(n)
        },
    }
}

// Get a single line address (if there is none).  If the beginning
// cannot parse as a line address, it will return None.  Otherwise, it
// will parse as much as can possibly be used to create a term of the
// any of the forms (where NUM is a decimal integer):
//
// .
// $
// +
// -
// NUM
// -NUM
// +NUM
// NUM+
// NUM-
// NUM+NUM
// NUM-NUM
// .+NUM
// .-NUM
// .+
// .-
// $+NUM
// $-NUM
// $+
// $-
//
fn get_line_number(state: &mut State) -> io::Result<Option<usize>>
{
    state.skip_ws()?;
    if let Some(c) = state.current_char()? {
        let term1 =
            match c {
                '.' => {
                    state.skip_char()?;
                    state.current_line
                },
                '$' => {
                    state.skip_char()?;
                    state.last_line()
                },
                '\'' => {
                    state.skip_char()?;
                    if let Some(c) = state.current_char()? {
                        if c >= 'a' && c <= 'z' {
                            state.skip_char()?;

                            let mut l = 0;
                            for i in 1 ..= state.last_line() {
                                if let Some(m) = state.lines[i - 1].mark_char {
                                    if m == c {
                                        l = i;
                                        break;
                                    }
                                }
                            }
                            if l > 0 {
                                l
                            } else {
                                return err(io::ErrorKind::InvalidData, "mark not found");
                            }
                        } else {
                            return err(io::ErrorKind::InvalidData, "invalid mark name");
                        }
                    } else {
                        return err(io::ErrorKind::InvalidData, "invalid mark name");
                    }
                },
                '/' => get_scan_pattern(state, c)?,
                '?' => get_scan_pattern(state, c)?,
                '0'..='9' => {
                    let acc = get_num(state, c)?;
                    acc
                },
                '+' | '-' => {
                    let sign = c;
                    state.skip_char()?;
                    if let Some(c) = state.current_char()? {
                        match c {
                            '0'..='9' => {
                                let acc = get_num(state, c)?;
                                return get_rel_line(state, sign, state.current_line, acc)
                            },
                            _ => return get_rel_line(state, sign, state.current_line, 1)
                        }
                    } else {
                        return get_rel_line(state, sign, state.current_line, 1)
                    }
                },
                _ => {
                    return Ok(None)
                }
            };
        state.skip_ws()?;
        if let Some(c) = state.current_char()? {
            if c == '+' || c == '-' {
                let sign = c;
                state.skip_char()?;
                let increment =
                    if let Some(c) = state.current_char()? {
                        match c {
                            '0'..='9' => {
                                get_num(state, c)?
                            },
                            _ => 1
                        }
                    } else {
                        1
                    };
                get_rel_line(state, sign, term1, increment)
            } else {
                Ok(Some(term1))
            }
        } else {
            Ok(Some(term1))
        }

   } else {
       Ok(None)
   }
}

fn get_lines(state: &mut State) -> io::Result<Lines>
{

    // Get optional first line address.
    let line1 = get_line_number(state)?;

    // Do some bounds checking here so we don't have to repeat it
    // below for different cases.
    if let Some(l1) = line1 {
        if l1 > state.last_line() {
            return address_err();
        }
    }

    // Now check for second line address.
    state.skip_ws()?;
    if let Some(c) = state.current_char()? {
        if c == ',' || c == ';' {
            state.skip_char()?;

            if c == ';' {
                if let Some(l1) = line1 {
                    state.current_line = l1;
                }
            }

            // Get optional second line address.
            let line2 = get_line_number(state)?;

            // Now fill in defaults.  If second address is given, we
            // check its bounds.
            match (line1, line2) {
                (None, None) => Ok(Some((1, state.last_line()))),
                (Some(l1), None) => Ok(Some((l1, l1))),
                (None, Some(l2)) => Ok(Some((l2, l2))),
                (Some(l1), Some(l2)) =>
                    if l1 <= l2 && l2 <= state.last_line() {
                        Ok(Some((l1, l2)))
                    } else {
                        address_err()
                    }
            }
        } else {
            // No second address given, so we fill in defaults
            // (command probably follows in input).
            match line1 {
                None => Ok(None),
                Some(l1) => Ok(Some((l1, l1))),
            }
        }
    } else {
        //No second address given, so we fill in defaults (end of
        // input).
        match line1 {
            None => Ok(None),
            Some(l1) => Ok(Some((l1, l1))),
        }
    }
}

fn get_argument(state: &mut State) -> io::Result<Option<String>> {
    state.skip_ws()?;
    if state.current_char()?.is_some() {
        let mut arg = String::new();
        loop {
            if let Some(c) = state.current_char()? {
                if c == '\n' {
                    break;
                }
                arg.push(c);
                state.skip_char()?;
            } else {
                break;
            }
        }
        if arg.len() > 0 {
            Ok(Some(arg))
        } else {
            Ok(None)
        }
    } else {
        Ok(None)
    }
}

fn get_char_class(state: &mut State) -> io::Result<String> {
    let mut class = String::new();
    while let Some(c) = state.current_char()? {
        match c {
            ']' => {
                state.skip_char()?;
                if class.len() == 0 {
                    class.push(c);
                } else {
                    break;
                }
            },
            '-' => {
                state.skip_char()?;
                if class.len() == 0 {
                    class.push(c);
                } else {
                    class.push(c);
                }
            },
            '\n' => {
                break;
            },
            _ => {
                state.skip_char()?;
                class.push(c);
            },
        }
    }
    Ok(class)
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Pat {
    Any,
    Char(char),
    Class(String),
    Star(Box<Pat>),
    Bol,
    Eol,
}


fn amatch(pat: &Vec<Pat>, mut pi: usize, target: &Vec<char>, mut ti: usize) ->
    Option<(usize, usize)> {
    let tstart = ti;
    loop {
        if pi == pat.len() {
            return Some((tstart, ti));
        }
        match &pat[pi] {
            Pat::Bol =>
                if ti > 0 {
                    break;
                } else {
                    pi += 1;
                },
            Pat::Eol =>
                if ti < target.len() {
                    break;
                } else {
                    pi += 1;
                },
            Pat::Char(c) =>
                if *c != target[ti] {
                    break;
                } else {
                    pi += 1;
                    ti += 1;
                },
            Pat::Any =>
                if target[ti] == '\n' {
                    break;
                } else {
                    pi += 1;
                    ti += 1;
                },
            _ =>
                break,
        }
    }
    None
}

fn match_pattern(cpat: &Vec<Pat>, target0: &str) -> Option<usize> {
    let target = target0.chars().collect::<Vec<char>>();

    for i in 0..target.len() {
        if let Some((m, _)) = amatch(cpat, 0, &target, i) {
            return Some(m)
        }
    }
    None
}

fn get_atomic_pattern(state: &mut State) -> io::Result<Pat>
{
    if let Some(c) = state.current_char()? {
        if c == '\\' {
            state.skip_char()?;
            if let Some(c) = state.current_char()? {
                state.skip_char()?;
                Ok(Pat::Char(c))
            } else {
                invalid_pattern_err()
            }
        } else if c == '[' {
            state.skip_char()?;
            let class = get_char_class(state)?;
            Ok(Pat::Class(class))
        } else if c == '.' {
            state.skip_char()?;
            Ok(Pat::Any)
        } else if c == '^' {
            state.skip_char()?;
            Ok(Pat::Bol)
        } else if c == '$' {
            state.skip_char()?;
            Ok(Pat::Eol)
        } else {
            state.skip_char()?;
            Ok(Pat::Char(c))
        }
    } else {
        invalid_pattern_err()
    }
}

fn get_pattern1(state: &mut State, delim: char) -> io::Result<Vec<Pat>>
{
    let mut pattern = Vec::new();

    while let Some(c) = state.current_char()? {
        if c == delim {
            break;
        } else if c == '\n' {
            break;
        } else if c == ')' {
            break;
        } else if c == '|' {
            break;
        } else {
            let mut sub_pat = get_atomic_pattern(state)?;
            if let Some(c) = state.current_char()? {
                if c == '*' {
                    state.skip_char()?;
                    sub_pat = Pat::Star(Box::new(sub_pat));
                }
            }
            pattern.push(sub_pat);
        }
    }
    Ok(pattern)
}

fn get_pattern(state: &mut State) -> io::Result<Vec<Pat>> {
    state.skip_ws()?;

    if let Some(c) = state.current_char()? {
        let delim = c;

        state.skip_char()?;

        let pat = get_pattern1(state, delim)?;

        for (i, p) in pat.iter().enumerate() {
            match p {
                Pat::Bol =>
                    if i > 0 {
                        return invalid_pattern_err();
                    },
                Pat::Eol =>
                    if i < pat.len() - 1 {
                        return invalid_pattern_err();
                    },
                _ => {},
            }
        }

        // The closing delimiter is optional and will be skipped, if
        // present.
        if let Some(c) = state.current_char()? {
            if c == delim {
                state.skip_char()?;
            }
        }
        Ok(pat)
    } else {
        missing_pattern_err()
    }
}

// Quit command (q).  Ensures that no line addresses and no parameters
// are given and returns false, which will terminate the main loop.
fn quit_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    any_address_err(l)?;
    any_arg_err(state)?;

    if state.modified && state.last_command != Some('q') {
        return err(io::ErrorKind::Other, "buffer modified")
    }

    Ok(false)
}

// Print command (p).  Uses the current line as defaults if no
// addresses are given.  Verifies that the start address is greater
// than zero and that no parameters are given.  Then prints all lines
// in the line range to the terminal and set the current line to the
// last line printed.
fn print_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    let (line1, line2) = default_lines(l, state.current_line, state.current_line);

    zero_address_err(line1)?;
    any_arg_err(state)?;

    do_print(state, line1, line2, Some(PrintFlag::Print));

    state.current_line = line2;
    Ok(true)
}

// List command (l). Print each line unambiguously, that means at the
// end of each line, a dollar ($) character is printed.  All dollar
// characters that appear on lines are prefixed with a backslash (\)
// character.
fn list_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    let (line1, line2) = default_lines(l, state.current_line, state.current_line);

    zero_address_err(line1)?;
    any_arg_err(state)?;

    do_print(state, line1, line2, Some(PrintFlag::List));

    state.current_line = line2;
    Ok(true)
}

// Enumerate command (n). Print each line in the address range,
// prefixing each line with it's line number.
fn enum_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    let (line1, line2) = default_lines(l, state.current_line, state.current_line);

    zero_address_err(line1)?;
    any_arg_err(state)?;

    do_print(state, line1, line2, Some(PrintFlag::Enumerate));

    state.current_line = line2;
    Ok(true)
}

fn load(filename: &Option<String>) -> io::Result<(usize, Vec<String>)> {
    if let Some(fname) = filename {
        let mut size = 0;
        let mut lines = Vec::new();
        let mut f = BufReader::new(File::open(fname)?);

        for s in f.lines() {
            let str = s?;
            size += str.len() + 1;
            lines.push(str.clone());
        }
        Ok((size, lines))
    } else {
        err(io::ErrorKind::InvalidData, "no file name given")
    }
}

fn save(state: &State, filename: &Option<String>, line1: usize, line2: usize) -> io::Result<usize> {
    if let Some(fname) = filename {
        let mut f = BufWriter::new(File::create(fname)?);
        let mut size = 0;
        for l in state.lines.as_slice()[line1 - 1..line2].iter() {
            f.write_all(l.text.as_bytes())?;
            f.write_all(b"\n")?;
            size += l.text.len() + 1;
        }
        Ok(size)
    } else {
        err(io::ErrorKind::InvalidData, "no file name given")
    }
}

fn input_mode(state: &mut State) -> io::Result<Vec<String>> {
    state.mode = Mode::Input;
    let mut lines = Vec::new();

    while let Some(mut s) = state.read_line()? {
        if s == ".\n" {
            break;
        }
        let _ = s.pop();
        lines.push(s.clone());
    }
    Ok(lines)
}

// Append command (a).  Read in lines from standard input until a line
// with a single period (.) is read.  The line terminating input is
// discarded.  The other lines are inserted into the buffer after the
// addressed line.  The current line is set to the last line inserted.
fn append_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    let (_line1, insert_line) = default_lines(l, state.current_line, state.current_line);

    // Note that line address 0 is allowed, to insert at the start of
    // the buffer.
    let pflag = print_flag(state)?;
    any_arg_err(state)?;

    // Read lines from stdin until a line with a single period (.) is
    // read.
    let lines = input_mode(state)?;

    let line_count = lines.len();

    // Insert into buffer after the given first line address.
    state.append(insert_line, lines);

    state.current_line = insert_line + line_count;
    do_print(state, state.current_line, state.current_line, pflag);
    Ok(true)
}

// Insert command (i).  Read in lines from standard input until a line
// with a single period (.) is read.  The line terminating input is
// discarded.  The other lines are inserted into the buffer before the
// addressed line.  The current line is set to the last line inserted.
fn insert_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    let (_line1, insert_line0) = default_lines(l, state.current_line, state.current_line);

    // Note that line address 0 is allowed, it is equivalent to 1.
    let insert_line = std::cmp::max(insert_line0, 1);

    let pflag = print_flag(state)?;
    any_arg_err(state)?;

    // Read lines from stdin until a line with a single period (.) is
    // read.
    let lines = input_mode(state)?;

    let line_count = lines.len();

    // Insert into buffer after the given first line address.
    state.append(insert_line - 1, lines);

    state.current_line = insert_line + line_count - 1;
    do_print(state, state.current_line, state.current_line, pflag);
    Ok(true)
}

// Change command (c).  Read in lines from standard input until a line
// with a single period (.) is read.  The line terminating input is
// discarded.  The addresses lines are removed from the buffer and the
// newly read lines are inserted in their place.  The current line is
// set to the last line inserted.
fn change_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    let (line1, line2) = default_lines(l, state.current_line, state.current_line);

    zero_address_err(line1)?;
    let pflag = print_flag(state)?;
    any_arg_err(state)?;

    let lines = input_mode(state)?;

    state.delete(line1, line2);

    let line_count = lines.len();

    state.append(line1 - 1, lines);

    state.current_line = line1 + line_count - 1;
    do_print(state, state.current_line, state.current_line, pflag);
    Ok(true)
}

// Move command (m). Move the addressed lines after the line given as
// an argument. The current line is set to the last line moved.
fn move_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    let (line1, line2) = default_lines(l, state.current_line, state.current_line);

    if let Some(mut to_line) = get_line_number(state)? {

        if to_line >= line1 {
            to_line -= (line2 - line1) + 1;
        }

        zero_address_err(line1)?;
        let pflag = print_flag(state)?;
        any_arg_err(state)?;

        let lines = state.take(line1, line2);

        let line_count = lines.len();

        state.append(to_line, lines);

        state.current_line = to_line + line_count;
        do_print(state, state.current_line, state.current_line, pflag);
        Ok(true)
    } else {
        return missing_address_err();
    }
}

// Transfer command (t). Copy the addressed lines and insert them
// after the line given as an argument.  Set the current line to the
// last line copied.
fn transfer_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    let (line1, line2) = default_lines(l, state.current_line, state.current_line);

    if let Some(to_line) = get_line_number(state)? {

        zero_address_err(line1)?;
        let pflag = print_flag(state)?;
        any_arg_err(state)?;

        let lines = state.copy(line1, line2);

        let line_count = lines.len();

        state.append(to_line, lines);

        state.current_line = to_line + line_count;
        do_print(state, state.current_line, state.current_line, pflag);
        Ok(true)
    } else {
        return missing_address_err();
    }
}

// Delete command (d).  Remove the addressed line from the buffer.
// Set the current line to the line after the deleted lines, if it
// exists and to the line before otherwise.
fn delete_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    let (line1, line2) = default_lines(l, state.current_line, state.current_line);

    zero_address_err(line1)?;
    let pflag = print_flag(state)?;
    any_arg_err(state)?;

    state.delete(line1, line2);

    state.current_line = std::cmp::min(state.last_line(), line1);
    do_print(state, state.current_line, state.current_line, pflag);
    Ok(true)
}

// Join command (j). Join all lines in the given range, or if only one
// line is given, join that line with the following one.
fn join_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    let (line1, mut line2) = default_lines(l, state.current_line, state.current_line);

    zero_address_err(line1)?;
    let pflag = print_flag(state)?;
    any_arg_err(state)?;

    if line1 == line2 {
        if line1 == state.last_line() {
            return Ok(true);
        }
        line2 = line1 + 1;
    }
    let lines = state.take(line1, line2);

    let mut new_line = String::new();
    for l in lines {
        new_line.extend(l.chars());
    }

    state.append(line1 - 1, vec![new_line]);

    do_print(state, line1, line1, pflag);

    state.current_line = line1;
    Ok(true)
}

// Copy command (x). Insert contents of the cut buffer after the
// addressed line.  The current address is set to the last line
// copied.
fn copy_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    let (line1, line2) = default_lines(l, state.current_line, state.current_line);

    zero_address_err(line1)?;
    let pflag = print_flag(state)?;
    any_arg_err(state)?;


    if let Some(lines) = state.copy_cut_buffer() {
        let line_cnt = lines.len();

        state.append(line2, lines);

        state.current_line = line2 + line_cnt;

        do_print(state, state.current_line, state.current_line, pflag);
        Ok(true)
    } else {
        err(io::ErrorKind::InvalidData, "cut buffer empty")
    }
}

// Yank command (y). Copy (yank) the addressed lines to the cut
// buffer.  The current addres is not changed.
fn yank_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    let (line1, line2) = default_lines(l, state.current_line, state.current_line);

    zero_address_err(line1)?;
    any_arg_err(state)?;

    state.yank(line1, line2);

    Ok(true)
}

// Edit command (e).  Accepts an optional file name argument.  Opens
// the file with the given name (or the current default filename if no
// argument is given) and reads it into the edit buffer.  If the
// buffer is modified, an error is returned.  If a filename is given,
// it is used as the new default filename.
fn edit_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    any_address_err(l)?;

    if state.modified && state.last_command != Some('e') {
        return err(io::ErrorKind::Other, "buffer modified")
    }

    let fname = get_argument(state)?.or(state.default_filename.clone());

    let (size, lines) = load(&fname)?;
    state.set_lines(lines);
    state.modified = false;
    state.default_filename = fname;
    println!("{}", size);
    Ok(true)
}

// Read command (r).  Accepts an optional file name argument.  Opens
// the file with the given name (or the current default filename if no
// argument is given) and inserts it into the edit buffer at the given
// line (or current line, if no line is given).  The default filename
// is unchanged.
fn read_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    let (_line1, line2) = default_lines(l, state.last_line(), state.last_line());

    let fname = get_argument(state)?.or(state.default_filename.clone());

    let (size, lines) = load(&fname)?;
    state.insert_lines(line2, lines);
    println!("{}", size);

    Ok(true)
}

// Filename command (f).  If no argument is given, print the current
// default filename or return an error if no default filename is
// present. If an argument is given, set the default filename to the
// argument.
fn filename_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    any_address_err(l)?;

    let fname = get_argument(state)?;
    match fname {
        None =>
            match &state.default_filename {
                None =>
                    return err(io::ErrorKind::Other, "no filename"),
                Some(f) =>
                    println!("{}", f),
            },
        Some(_) =>
            state.default_filename = fname,
    }

    Ok(true)
}

// Write command (w).  Accepts an optional file name argument.  Write
// the addressed lines (or the complete buffer contents if no lines
// are specified) to the given file (or the default filename if none
// is given) and mark the buffer as unmodified.
fn write_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    let (line1, line2) = default_lines(l, 1, state.last_line());

    let fname = get_argument(state)?.or(state.default_filename.clone());

    let size = save(state, &fname, line1, line2)?;
    state.modified = false;
    state.default_filename = fname;
    println!("{}", size);
    Ok(true)
}

// Prompt command (P).  Set the prompt to the given argument if given
// or toggle prompt printing otherwise.
fn prompt_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    any_address_err(l)?;

    let new_prompt = get_argument(state)?;
    match new_prompt {
        None =>
            state.print_prompt = !state.print_prompt,
        Some(_) =>
            state.prompt = new_prompt,
    }

    Ok(true)
}

// Work horse for the g and v commands.
fn global_execute(state: &mut State, l: Lines, include: bool) -> io::Result<bool>
{
    let (line1, line2) = default_lines(l, 1, state.last_line());

    let pat = get_pattern(state)?;

    state.unmark();

    for line in line1 ..= line2 {
        let matches = match_pattern(&pat, &state.lines[line - 1].text);
        if matches.is_some() && include {
            state.lines[line - 1].mark = true;
        } else if matches.is_none() && !include {
            state.lines[line - 1].mark = true;
        }
    }

    state.exec_global = true;

    let in_line = state.take_to_end()?;
    let cmd: Vec<_> = in_line.lines().collect();

    // No for loop here, because lines can be added or deleted by the
    // executed commands.
    let mut line = 1;
    while line <= state.last_line() {
        if state.lines[line - 1].mark {
            state.current_line = line;

            state.append_input_lines(cmd.iter().map(|x|
                                                    {
                                                        let mut s = x.to_string();
                                                        s.push('\n');
                                                        s
                                                    }).collect());
            run_one(state)?;
            state.input_lines.clear();
        }
        line += 1;
    }
    state.exec_global = false;
    Ok(true)
}

// Global command (g).  Mark a set of lines matching the argument and
// then execute the following commands on all of the marked lines.
fn global_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    global_execute(state, l, true)
}

// Global exclusion command (v).  Mark a set of lines NOT matching the
// argument and then execute the following commands on all of the
// marked lines.
fn vlobal_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    global_execute(state, l, false)
}

// Mark command (k).  Marks the addressed line with the letter given
// as an argument.
fn mark_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    let (_line1, line2) = default_lines(l, state.current_line, state.current_line);

    let arg = get_argument(state)?;
    match arg {
        None =>
            return missing_argument_err(),
        Some(m) =>
            if m.len() != 1 {
                return err(io::ErrorKind::InvalidData, "invalid mark name");
            } else {
                state.mark_line(line2, m.chars().next().unwrap());
            },
    }

    Ok(true)
}

// Print line command (=).  Defaults lines to the last line and
// verifies that no parameters are given.  Then prints the number of
// the last line address.
fn print_line_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    let (_line1, line2) = default_lines(l, state.last_line(), state.last_line());

    any_arg_err(state)?;

    println!("{}", line2);
    Ok(true)
}

// Empty command (empty input line or only line addresses).  If lines
// are given, it sets the current line to the last line, otherwise, it
// prints the current line and increments the current line number,
// wrapping around to the first line if necessary.
fn empty_cmd(state: &mut State, l: Lines) -> io::Result<bool>
{
    any_arg_err(state)?;

    match l {
        None => {
            state.next_line();
            print_cmd(state, None)?;
            Ok(true)
        },
        Some((_l1, l2)) => {
            state.current_line = l2;
            print_cmd(state, None)?;
            Ok(true)
        },
    }
}

fn do_print(state: &State, line1: usize, line2: usize, pflag: Option<PrintFlag>) {
    match pflag {
        Some(PrintFlag::Print) => {
            for l in line1..=line2 {
                println!("{}", state.lines[l - 1].text);
            }
        },
        Some(PrintFlag::Enumerate) => {
            for l in line1..=line2 {
                println!("{} {}",
                         l,
                         state.lines[l-1].text);
            }
        },
        Some(PrintFlag::List) => {
            for l in line1..=line2 {
                for c in state.lines[l - 1].text.chars() {
                    match c {
                        '$' => print!("\\$"),
                        '\t' => print!(">"),
                        '\x08' => print!("<"),
                        _ =>
                            if c < ' ' {
                                // Formula taken from 2.11BSD ed (see
                                // https://minnie.tuhs.org//cgi-bin/utree.pl?file=2.11BSD/src/bin/ed.c)
                                let l1 = (((c as u8) >> 3) + ('0' as u8)) as char;
                                let l2 = (((c as u8) & 0x7) + ('0' as u8)) as char;
                                print!("\\{}{}", l1, l2);
                            } else {
                                print!("{}", c);
                            },
                    }
                }
                println!("$");
            }
        },
        None => {},
    }
}

fn run_one(state: &mut State) -> io::Result<bool>
{
    state.mode = Mode::Command;
    let lines = get_lines(state)?;
    state.skip_ws()?;

    if let Some(c) = state.current_char()? {
        state.skip_char()?;
        let result =
            match c {
                'q' => quit_cmd(state, lines),
                'p' => print_cmd(state, lines),
                'l' => list_cmd(state, lines),
                'n' => enum_cmd(state, lines),
                'a' => append_cmd(state, lines),
                'i' => insert_cmd(state, lines),
                'c' => change_cmd(state, lines),
                'm' => move_cmd(state, lines),
                't' => transfer_cmd(state, lines),
                'd' => delete_cmd(state, lines),
                'j' => join_cmd(state, lines),
                'e' => edit_cmd(state, lines),
                'f' => filename_cmd(state, lines),
                'w' => write_cmd(state, lines),
                'r' => read_cmd(state, lines),
                'k' => mark_cmd(state, lines),
                'x' => copy_cmd(state, lines),
                'y' => yank_cmd(state, lines),
                '=' => print_line_cmd(state, lines),
                '\n' => empty_cmd(state, lines),
                _   => {
                    err(io::ErrorKind::InvalidData, "unknown command")
                },
            };
        result
    } else {
        print_cmd(state, lines)
    }
}

fn run(state: &mut State) -> io::Result<bool> {
    state.mode = Mode::Command;
    let lines = get_lines(state)?;
    state.skip_ws()?;

    if let Some(c) = state.current_char()? {
        let mut last_cmd = Some(c);
        state.skip_char()?;
        let result =
            match c {
                'q' => quit_cmd(state, lines),
                'p' => print_cmd(state, lines),
                'l' => list_cmd(state, lines),
                'n' => enum_cmd(state, lines),
                'a' => append_cmd(state, lines),
                'i' => insert_cmd(state, lines),
                'c' => change_cmd(state, lines),
                'm' => move_cmd(state, lines),
                't' => transfer_cmd(state, lines),
                'd' => delete_cmd(state, lines),
                'j' => join_cmd(state, lines),
                'e' => edit_cmd(state, lines),
                'f' => filename_cmd(state, lines),
                'w' => write_cmd(state, lines),
                'r' => read_cmd(state, lines),
                'g' => global_cmd(state, lines),
                'v' => vlobal_cmd(state, lines),
                'P' => prompt_cmd(state, lines),
                'k' => mark_cmd(state, lines),
                'x' => copy_cmd(state, lines),
                'y' => yank_cmd(state, lines),
                '=' => print_line_cmd(state, lines),
                '\n' => empty_cmd(state, lines),
                _   => {
                    err(io::ErrorKind::InvalidData, "unknown command")
                },
            };
        state.last_command = last_cmd;
        result
    } else {
        Ok(false)
    }
}

fn main() -> io::Result<()> {
    let mut state = State::new();
    let mut prompt_arg = false;
    for arg in std::env::args().skip(1) {
        if  arg == "-p" {
            state.print_prompt = true;
            prompt_arg = true;
        } else if arg == "-" {
            // Ignore "-"
        } else if arg == "" {
            break;
        } else {
            let fname = arg.clone();
            if prompt_arg {
                state.prompt = Some(arg.clone());
                prompt_arg = false;
            } else {
                match load(&Some(fname.clone())) {
                    Ok((size, lines)) => {
                        state.set_lines(lines);
                        state.default_filename = Some(fname);
                        println!("{}", size);
                    },
                    Err(e) =>  {
                        eprintln!("{}: {}", fname, e);

                        // If the file does not exist, we remember the
                        // filename because the user might want to create
                        // a new file.
                        if e.kind() == io::ErrorKind::NotFound {
                            state.default_filename = Some(fname);
                        }
                    },
                }
                break;
            }
        }
    }

    // Main loop. Read top-level commands an execute them until one of
    // them returns false (indicating that the loop should not
    // continue).
    loop {
        match run(&mut state) {
            Ok(true) => {
            },
            Ok(false) => {
                break;
            },
            Err(e) =>
                println!("? {}", e),
        }
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    fn mk_input(buf: &str) -> Input<std::str::Chars> {
        Input::new(buf.chars())
    }

    #[test]
    fn simple_pattern() {
        let mut input = mk_input("/a/");
        let expected = vec![Pat::Char('a')];
        let p = get_pattern(&mut input).unwrap();
        assert_eq!(p, expected);
    }

    #[test]
    fn empty_pattern() {
        let mut input = mk_input("//");
        let expected = vec![];
        let p = get_pattern(&mut input).unwrap();
        assert_eq!(p, expected);
    }

    #[test]
    fn back_pattern() {
        let mut input = mk_input("?abc?");
        let expected = vec![Pat::Char('a'), Pat::Char('b'), Pat::Char('c')];
        let p = get_pattern(&mut input).unwrap();
        assert_eq!(p, expected);
    }

    #[test]
    fn bol_anchor_pattern() {
        let mut input = mk_input("/^a/");
        let expected = vec![Pat::Bol, Pat::Char('a')];
        let p = get_pattern(&mut input).unwrap();
        assert_eq!(p, expected);
    }

    #[test]
    fn bol_anchor_pattern_error() {
        let mut input = mk_input("/a^/");
        assert!(get_pattern(&mut input).is_err());
    }

    #[test]
    fn eol_anchor_pattern() {
        let mut input = mk_input("/a$/");
        let expected = vec![Pat::Char('a'), Pat::Eol];
        let p = get_pattern(&mut input).unwrap();
        assert_eq!(p, expected);
    }

    #[test]
    fn eol_anchor_pattern_error() {
        let mut input = mk_input("/$a/");
        assert!(get_pattern(&mut input).is_err());
    }

}
