use std::io::{self, Write, BufRead, BufReader, BufWriter};
use std::fs::{File};

type Lines = Option<(usize, usize)>;

struct Line {
    text: String,
}

impl Line {
    fn new(s: String) -> Self {
        Line {
            text: s,
        }
    }
}

struct State {
    default_filename: Option<String>,
    lines: Vec<Line>,
    current_line: usize,
    modified: bool,
    last_command: Option<char>,
    prompt: Option<String>,
    print_prompt: bool,
}

impl State {
    fn new() -> Self {
        State{
            default_filename: None,
            lines: Vec::new(),
            current_line: 0,
            modified: false,
            last_command: None,
            prompt: Some("*".into()),
            print_prompt: false
        }
    }

    fn clear_buffer(&mut self) {
        self.lines.clear();
        self.current_line = 0;
        self.modified = true;
    }
    
    fn set_lines(&mut self, l: Vec<String>) {
        self.lines.clear();
        for s in l {
            self.lines.push(Line::new(s.into()));
        }
        self.current_line = self.last_line();
        self.modified = false;
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
    
    fn last_line(&self) -> usize {
        self.lines.len()
    }

    fn next_line(&mut self) {
        self.current_line += 1;
        if self.current_line > self.last_line() {
            self.current_line = 1;
        }
    }

}

struct Input<I: Iterator<Item=char>> {
    iter: I,
    lookahead: Option<char>,
}

impl<I: Iterator<Item=char>> Input<I> {
    fn new(mut iter: I) -> Self {
        let c = iter.next();
        Input{
            iter: iter,
            lookahead: c,
        }
    }

    fn current(&mut self) -> Option<char> {
        self.lookahead
    }

    fn skip(&mut self) {
        self.lookahead = self.iter.next();
    }

    fn skip_ws(&mut self) {
        while let Some(c) = self.current() {
            if c == ' ' || c == '\t' {
                self.skip();
            } else {
                return;
            }
        }
    }

    fn skip_all_ws(&mut self) {
        while let Some(c) = self.current() {
            if c == ' ' || c == '\t' || c == '\r' || c == '\n' {
                self.skip();
            } else {
                return;
            }
        }
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

// Return an error value if the given input contains any remaining
// non-whitespace characters.  Will also ignore newlines and carriage
// returns.
fn any_input_err<I: Iterator<Item=char>>(input: &mut Input<I>) -> io::Result<()> {
    input.skip_all_ws();
    match input.current() {
        None => Ok(()),
        Some(_) =>
            err(io::ErrorKind::InvalidData, "no parameters allowed")
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
    if state.print_prompt {
        match &state.prompt {
            None => {
                return err(io::ErrorKind::Other, "no prompt set");
            },
            Some(p) => {
                print!("{}", p);
                stdout.flush()?
            },
        }
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

// Parse a decimal integer from the input. Starts with the character
// `c`, which must be in the range '0'..'9' (inclusive).  Will stop as
// soon as a non-numeric character is found (or a the end of input).
// Returns the result wrapped in a `io::Result` for convenience.
fn get_num<I: Iterator<Item=char>>(input: &mut Input<I>, c: char) -> io::Result<usize> {
    let mut acc = atoi(c);
    input.skip();
    while let Some(c) = input.current() {
        match c {
            '0'..='9' => {
                acc = acc * 10 + atoi(c);
                input.skip();
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
fn get_line_number<I: Iterator<Item=char>>(state: &State, input: &mut Input<I>) -> io::Result<Option<usize>> {
    input.skip_ws();
    if let Some(c) = input.current() {
        let term1 =
            match c {
                '.' => {
                    input.skip();
                    state.current_line
                },
                '$' => {
                    input.skip();
                    state.last_line()
                },
                '0'..='9' => {
                    let acc = get_num(input, c)?;
                    acc
                },
                '+' | '-' => {
                    let sign = c;
                    input.skip();
                    if let Some(c) = input.current() {
                        match c {
                            '0'..='9' => {
                                let acc = get_num(input, c)?;
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
        input.skip_ws();
        if let Some(c) = input.current() {
            if c == '+' || c == '-' {
                let sign = c;
                input.skip();
                let increment =
                    if let Some(c) = input.current() {
                        match c {
                            '0'..='9' => {
                                get_num(input, c)?
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

fn get_lines<I: Iterator<Item=char>>(state: &mut State, input: &mut Input<I>) -> io::Result<Lines> {

    // Get optional first line address.
    let line1 = get_line_number(state, input)?;

    // Do some bounds checking here so we don't have to repeat it
    // below for different cases.
    if let Some(l1) = line1 {
        if l1 > state.last_line() {
            return address_err();
        }
    }

    // Now check for second line address.
    input.skip_ws();
    if let Some(c) = input.current() {
        if c == ',' {
            input.skip();

            // Get optional second line address.
            let line2 = get_line_number(state, input)?;

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
 
fn get_argument<I: Iterator<Item=char>>(input: &mut Input<I>) -> io::Result<Option<String>> {
    input.skip_ws();
    if input.current().is_some() {
        let mut arg = String::new();
        loop {
            if let Some(c) = input.current() {
                if c == '\n' {
                    break;
                }
                arg.push(c);
                input.skip();
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

// Quit command (q).  Ensures that no line addresses and no parameters
// are given and returns false, which will terminate the main loop.
fn quit_cmd<I: Iterator<Item=char>>(state: &mut State, input: &mut Input<I>, l: Lines) ->
    io::Result<bool>
{
    any_address_err(l)?;
    any_input_err(input)?;

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
fn print_cmd<I: Iterator<Item=char>>(state: &mut State, input: &mut Input<I>, l: Lines) ->
    io::Result<bool>
{
    let (line1, line2) = default_lines(l, state.current_line, state.current_line);

    zero_address_err(line1)?;
    any_input_err(input)?;

    for l in line1..=line2 {
        println!("{}", state.lines[l - 1].text);
    }
    state.current_line = line2;
    Ok(true)
}

// List command (l). Print each line unambiguously, that means at the
// end of each line, a dollar ($) character is printed.  All dollar
// characters that appear on lines are prefixed with a backslash (\)
// character.
fn list_cmd<I: Iterator<Item=char>>(state: &mut State, input: &mut Input<I>, l: Lines) ->
    io::Result<bool>
{
    let (line1, line2) = default_lines(l, state.current_line, state.current_line);

    zero_address_err(line1)?;
    any_input_err(input)?;

    for l in line1..=line2 {
        for c in state.lines[l - 1].text.chars() {
            match c {
                '$' => print!("\\$"),
                _ => print!("{}", c),
            }
        }
        println!("$");
    }
    state.current_line = line2;
    Ok(true)
}

// Enumerate command (n). Print each line in the address range,
// prefixing each line with it's line number.
fn enum_cmd<I: Iterator<Item=char>>(state: &mut State, input: &mut Input<I>, l: Lines) ->
    io::Result<bool>
{
    let (line1, line2) = default_lines(l, state.current_line, state.current_line);

    zero_address_err(line1)?;
    any_input_err(input)?;

    for l in line1..=line2 {
        println!("{} {}", l, state.lines[l-1].text);
    }
    state.current_line = line2;
    Ok(true)
}

fn load(filename: &Option<String>) -> io::Result<Vec<String>> {
    if let Some(fname) = filename {
        let mut lines = Vec::new();
        let mut f = BufReader::new(File::open(fname)?);

        for s in f.lines() {
            lines.push(s?.clone());
        }
        Ok(lines)
    } else {
        err(io::ErrorKind::InvalidData, "no file name given")
    }
}

fn save(state: &State, filename: &Option<String>, line1: usize, line2: usize) -> io::Result<()> {
    if let Some(fname) = filename {
        let mut f = BufWriter::new(File::create(fname)?);

        for l in state.lines.as_slice()[line1 - 1..line2].iter() {
            f.write_all(l.text.as_bytes())?;
            f.write_all(b"\n")?;
        }
        Ok(())
    } else {
        err(io::ErrorKind::InvalidData, "no file name given")
    }
}

fn input_mode() -> io::Result<Vec<String>> {
    let mut lines = Vec::new();
    let mut buf = String::new();
    
    let stdin = io::stdin();

    let mut cnt = stdin.read_line(&mut buf)?;
    while cnt > 0 {
        if buf == ".\n" {
            break;
        }
        let _ = buf.pop();
        lines.push(buf.clone());
        buf.clear();
        cnt = stdin.read_line(&mut buf)?;
    }
    Ok(lines)
}

// Append command (a).  Read in lines from standard input until a line
// with a single period (.) is read.  The line terminating input is
// discarded.  The other lines are inserted into the buffer after the
// addressed line.  The current line is set to the last line inserted.
fn append_cmd<I: Iterator<Item=char>>(state: &mut State, input: &mut Input<I>, l: Lines) -> io::Result<bool> {
    let (_line1, insert_line) = default_lines(l, state.current_line, state.current_line);

    // Note that line address 0 is allowed, to insert at the start of
    // the buffer.
    any_input_err(input)?;

    // Read lines from stdin until a line with a single period (.) is
    // read.
    let lines = input_mode()?;

    let line_count = lines.len();
    
    // Insert into buffer after the given first line address.
    state.append(insert_line, lines);
    
    state.current_line = insert_line + line_count;
    Ok(true)
}

// Insert command (i).  Read in lines from standard input until a line
// with a single period (.) is read.  The line terminating input is
// discarded.  The other lines are inserted into the buffer before the
// addressed line.  The current line is set to the last line inserted.
fn insert_cmd<I: Iterator<Item=char>>(state: &mut State, input: &mut Input<I>, l: Lines) ->
    io::Result<bool>
{
    let (_line1, insert_line0) = default_lines(l, state.current_line, state.current_line);

    // Note that line address 0 is allowed, it is equivalent to 1.
    let insert_line = std::cmp::max(insert_line0, 1);
    
    any_input_err(input)?;

    // Read lines from stdin until a line with a single period (.) is
    // read.
    let lines = input_mode()?;

    let line_count = lines.len();
    
    // Insert into buffer after the given first line address.
    state.append(insert_line - 1, lines);
    
    state.current_line = insert_line + line_count - 1;
    Ok(true)
}

// Change command (c).  Read in lines from standard input until a line
// with a single period (.) is read.  The line terminating input is
// discarded.  The addresses lines are removed from the buffer and the
// newly read lines are inserted in their place.  The current line is
// set to the last line inserted.
fn change_cmd<I: Iterator<Item=char>>(state: &mut State, input: &mut Input<I>, l: Lines) ->
    io::Result<bool>
{
    let (line1, line2) = default_lines(l, state.current_line, state.current_line);

    zero_address_err(line1)?;
    any_input_err(input)?;

    let lines = input_mode()?;

    state.delete(line1, line2);
    
    let line_count = lines.len();
    
    state.append(line1 - 1, lines);
    
    state.current_line = line1 + line_count - 1;
    Ok(true)
}

// Move command (m). Move the addressed lines after the line given as
// an argument. The current line is set to the last line moved.
fn move_cmd<I: Iterator<Item=char>>(state: &mut State, input: &mut Input<I>, l: Lines) ->
    io::Result<bool>
{
    let (line1, line2) = default_lines(l, state.current_line, state.current_line);

    if let Some(mut to_line) = get_line_number(state, input)? {
    
        if to_line >= line1 {
            to_line -= (line2 - line1) + 1;
        }
    
        zero_address_err(line1)?;
        any_input_err(input)?;

        let lines = state.take(line1, line2);
        
        let line_count = lines.len();
        
        state.append(to_line, lines);
    
        state.current_line = to_line + line_count;
        Ok(true)
    } else {
        return err(io::ErrorKind::InvalidData, "line address missing");
    }
}

// Transfer command (t). Copy the addressed lines and insert them
// after the line given as an argument.  Set the current line to the
// last line copied.
fn transfer_cmd<I: Iterator<Item=char>>(state: &mut State, input: &mut Input<I>, l: Lines) ->
    io::Result<bool>
{
    let (line1, line2) = default_lines(l, state.current_line, state.current_line);

    if let Some(to_line) = get_line_number(state, input)? {
    
        zero_address_err(line1)?;
        any_input_err(input)?;

        let lines = state.copy(line1, line2);
        
        let line_count = lines.len();
        
        state.append(to_line, lines);
    
        state.current_line = to_line + line_count;
        Ok(true)
    } else {
        return err(io::ErrorKind::InvalidData, "line address missing");
    }
}

// Delete command (d).  Remove the addressed line from the buffer.
// Set the current line to the line after the deleted lines, if it
// exists and to the line before otherwise.
fn delete_cmd<I: Iterator<Item=char>>(state: &mut State, input: &mut Input<I>, l: Lines) ->
    io::Result<bool>
{
    let (line1, line2) = default_lines(l, state.current_line, state.current_line);

    zero_address_err(line1)?;
    any_input_err(input)?;

    state.delete(line1, line2);
    
    state.current_line = std::cmp::min(state.last_line(), line1);
    Ok(true)
}

// Edit command (e).  Accepts an optional file name argument.  Opens
// the file with the given name (or the current default filename if no
// argument is given) and reads it into the edit buffer.  If the
// buffer is modified, an error is returned.  If a filename is given,
// it is used as the new default filename.
fn edit_cmd<I: Iterator<Item=char>>(state: &mut State, input: &mut Input<I>, l: Lines) ->
    io::Result<bool>
{
    any_address_err(l)?;

    if state.modified && state.last_command != Some('e') {
        return err(io::ErrorKind::Other, "buffer modified")
    }

    let fname = get_argument(input)?.or(state.default_filename.clone());

    let lines = load(&fname)?;
    state.clear_buffer();
    state.set_lines(lines);
    state.default_filename = fname;
    Ok(true)
}

// Filename command (f).  If no argument is given, print the current
// default filename or return an error if no default filename is
// present. If an argument is given, set the default filename to the
// argument.
fn filename_cmd<I: Iterator<Item=char>>(state: &mut State, input: &mut Input<I>, l: Lines) ->
    io::Result<bool>
{
    any_address_err(l)?;

    let fname = get_argument(input)?;
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
fn write_cmd<I: Iterator<Item=char>>(state: &mut State, input: &mut Input<I>, l: Lines) ->
    io::Result<bool>
{
    let (line1, line2) = default_lines(l, 1, state.last_line());

    let fname = get_argument(input)?.or(state.default_filename.clone());

    save(state, &fname, line1, line2)?;
    state.modified = false;
    state.default_filename = fname;
    Ok(true)
}

// Prompt command (P).  Set the prompt to the given argument if given
// or toggle prompt printing otherwise.
fn prompt_cmd<I: Iterator<Item=char>>(state: &mut State, input: &mut Input<I>, l: Lines) ->
    io::Result<bool>
{
    any_address_err(l)?;

    let new_prompt = get_argument(input)?;
    match new_prompt {
        None =>
            state.print_prompt = !state.print_prompt,
        Some(_) =>
            state.prompt = new_prompt,
    }

    Ok(true)
}

// Print line command (=).  Defaults lines to the last line and
// verifies that no parameters are given.  Then prints the number of
// the last line address.
fn print_line_cmd<I: Iterator<Item=char>>(state: &mut State, input: &mut Input<I>, l: Lines) ->
    io::Result<bool>
{
    let (_line1, line2) = default_lines(l, state.last_line(), state.last_line());
    
    any_input_err(input)?;

    println!("{}", line2);
    Ok(true)
}

// Empty command (empty input line or only line addresses).  If lines
// are given, it sets the current line to the last line, otherwise, it
// prints the current line and increments the current line number,
// wrapping around to the first line if necessary.
fn empty_cmd<I: Iterator<Item=char>>(state: &mut State, input: &mut Input<I>, l: Lines) -> io::Result<bool> {
    any_input_err(input)?;

    match l {
        None => {
            print_cmd(state, input, None)?;
            state.next_line();
            Ok(true)
        },
        Some((_l1, l2)) => {
            state.current_line = l2;
            print_cmd(state, input, None)?;
            Ok(true)
        },
    }
}

fn run<I: Iterator<Item=char>>(state: &mut State, input: &mut Input<I>) -> io::Result<bool> {
    let lines = get_lines(state, input)?;
    input.skip_ws();

    if let Some(c) = input.current() {
        let mut last_cmd = Some(c);
        input.skip();
        let result =
            match c {
                'q' => quit_cmd(state, input, lines),
                'p' => print_cmd(state, input, lines),
                'l' => list_cmd(state, input, lines),
                'n' => enum_cmd(state, input, lines),
                'a' => append_cmd(state, input, lines),
                'i' => insert_cmd(state, input, lines),
                'c' => change_cmd(state, input, lines),
                'm' => move_cmd(state, input, lines),
                't' => transfer_cmd(state, input, lines),
                'd' => delete_cmd(state, input, lines),
                'e' => edit_cmd(state, input, lines),
                'f' => filename_cmd(state, input, lines),
                'w' => write_cmd(state, input, lines),
                'P' => prompt_cmd(state, input, lines),
                '=' => print_line_cmd(state, input, lines),
                _   => {
                    let res = empty_cmd(state, input, lines);
                    last_cmd = None;
                    res
                },
            };
        state.last_command = last_cmd;
        result
    } else {
        Ok(false)
    }
}

fn main() -> io::Result<()> {
    let mut buf = String::new();

    let stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut state = State::new();

    for arg in std::env::args().skip(1) {
        if arg.len() > 0 {
            let fname = Some(arg.clone());
            let lines = load(&fname)?;
            state.set_lines(lines);
            state.default_filename = fname;
            break;
        }
    }
    
    prompt(&state, &mut stdout)?;

    let mut cnt = stdin.read_line(&mut buf)?;
    while cnt > 0 {
        match run(&mut state, &mut Input::new(buf.chars())) {
            Ok(true) => {
            },
            Ok(false) => {
                break;
            },
            Err(e) =>
                println!("? {}", e),
        }
        
        buf.clear();
        prompt(&state, &mut stdout)?;
        cnt = stdin.read_line(&mut buf)?;
    }

    Ok(())
}
