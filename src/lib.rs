//! Argument parser inspired by Perl's Getopt::Long.
//!
//! Provides a likely familiar option parser for those who may be familiar with Getopt::Long from
//! Perl.  The specific options for Getopt::Long that were targeted are `qw(:config gnu_getopt
//! no_ignore_case no_auto_abbrev)`.
//!
//! Quickstart:
//!
//! ```
//! use getoptions_long::*;
//! let args: Vec<String> = std::env::args().collect();
//! // example invocation:
//! //
//! //    backup-profile --list -n --backup-dir=/mnt/usr1/backups --backup-dir \
//! //        "/mnt/usr1/backups test -v -- -f.txt
//! let prog = args[0].clone();
//! let args: Vec<String> = args.into_iter().skip(1).collect();
//! let usage =  || {
//!     let prog = std::path::Path::new(&prog).file_name().unwrap().to_string_lossy();
//!     println!("usage: {prog} [-hlcpnv] [-b BACKUP] [--] [TAR_ARGUMENTS]");
//!     println!("  backup firefox profiles on win32 from cygwin");
//!     println!("  -h,--help               display this usage");
//!     println!("  -b,--backup-dir BACKUP  output backup files to BACKUP (default '.')");
//!     println!("  -l,--list               list profiles and their paths");
//!     println!("  -n,--dry-run            perform dry run with commands printed");
//!     println!("  -p,--print              print Firefox configuration root");
//!     println!("  -v,--verbose            be noisier");
//! };
//!
//! let mut backup_dir = String::new();
//! let mut dry_run = false;
//! let mut free_args = vec![];
//! let mut list = false;
//! let mut print = false;
//! let mut verbose = false;
//!
//! let result = get_options(&mut [
//!     Opt::SubSwitch("help|h", &|_| { usage(); std::process::exit(0); }),
//!     Opt::Switch   ("list|l", &mut list),
//!     Opt::Switch   ("p|print", &mut print),
//!     Opt::Switch   ("dry-run|n", &mut dry_run),
//!     Opt::Arg      ("backup-dir|b", &mut backup_dir),
//!     Opt::Switch   ("verbose|v", &mut verbose),
//!     Opt::Free     (&mut free_args),
//! ], &args);
//!
//! if let Err(ref err) = result {
//!     eprintln!("ERROR: parsing command line arguments: {err}");
//!     //std::process::exit(1); // don't do this in a test
//! }
//!
//! // ...
//! ```
//!
//! Why another command line parser?  Well, I missed the options Getopt::Long can do.
//!
//! In comparison to existing crates, such as clap and gumdrop, this parser is declarative only at
//! the function invocation site (usually `get_options` if you have a fully realized list of
//! strings, or `get_options_env` for quick and dirty).  It also uses a //lot// of mutability,
//! which Rust exposes in excruciating detail.  You do not have to define your own type, use derive
//! macros, and hope that it does what you want (clap), or drop to a frustrating builder invocation
//! chain to enable last-argument precedence (clap), or end up in a frustrating API for using
//! callbacks (clap, gumdrop).  (I'm actually not sure it's possible to use callbacks with clap &
//! grumdrop, because my brain might just not understand the documentation)
//!
//! The main thing that callbacks allow is the ability to use mutually exclusive command line
//! arguments with last precedence, such as:
//!
//! ```
//! use getoptions_long::*;
//! let args = ["--foo", "--baz", "--bar", "--baz"];
//!
//! let command = std::cell::RefCell::new(String::new());
//!
//! let sub: &dyn for<'a> Fn(&'a str) = &|arg| {
//!     let mut command = command.borrow_mut();
//!     *command = format!("command-{}", arg);
//! };
//!
//! let result = get_options_str(&mut [
//!     Opt::SubSwitch("foo", sub),
//!     Opt::SubSwitch("bar", sub),
//!     Opt::SubSwitch("baz", sub),
//! ], &args);
//!
//! assert_eq!(result, Ok(()));
//! assert_eq!(command.into_inner(), "command-baz");
//! ```
//!
//! If you aren't familiar with the Rustonomicon (I definitely am not), you might be wondering why
//! it is quite unergonomic to need to use RefCell.  It is the only way to pass the same closure to
//! multiple switches (because we're violating the multiple mutable borrows rule).
//!
//! Q&A:
//!
//! 1. Is it fast?  Seems fast enough for my needs.
//! 2. How much does it allocate?  No idea.
//! 3. no_std?  No STDs, but I don't think compiling with no_std is in scope.
//! 4. Other Getopt::Long options?  Maybe.  I mostly went with what I go with and seems fairly
//!    consistent with GNU standards.
//! 5. Any benchmarks?  Nope.
//! 6. Dependencies?  std.  That's it.
//! 7. How long did it take you?  Not long enough that I actually released it.
//! 8. Rewrite Perl in Rust?  Have you looked at the source code?  My eyes go spirally when I see
//!    way too many macros and ifdefs.  I ain't doing that.  (unless...?)
//! 9. Why not clap or gumdrop?  See above.


// getoptions-long - library to parse command line inspired by perl's Getopt::Long
// Copyright (C) 2024  Ira Peach
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::slice::Iter;

/// Define options
pub enum Opt<'a> {
    /// Represents an option that is true if given.
    Switch(&'a str, &'a mut bool),
    /// Represents an option that takes an argument.
    Arg(&'a str, &'a mut String),
    /// Represents an option whose argument may be parsed via a callback.
    SubSwitch(&'a str, FnSubSwitch<'a>),
    /// Represents an option whose argument and value may be parsed via a callback.
    SubArg(&'a str, FnSubArg<'a>),
    /// Represents a set of arguments not bound to any options.
    Free(&'a mut Vec<String>),
}

pub type FnSubSwitch<'a> = &'a dyn Fn(&str);
pub type FnSubArg<'a> = &'a dyn Fn(&str,&str);

impl Debug for Opt<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Opt::Switch(x, b) => f.debug_tuple("Opt::Switch").field(x).field(b).finish(),
            Opt::Arg(x, s) => f.debug_tuple("Opt::Arg").field(x).field(s).finish(),
            Opt::SubArg(x, _) => f.debug_tuple("Opt::SubArg").field(x).field(&"Fn").finish(),
            Opt::SubSwitch(x, _) => f.debug_tuple("Opt::SubSwitch").field(x).field(&"Fn").finish(),
            Opt::Free(v) => f.debug_tuple("Opt::Free").field(v).finish(),
        }
    }
}

#[derive(Clone,Debug,Eq,PartialEq)]
pub enum Error {
    NeedArgument(String),
    Unexpected(String),
}


impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Error::NeedArgument(s) => write!(f, "need argument for '{}'", s),
            Error::Unexpected(s) => write!(f, "unexpected argument '{}'", s),
        }
    }
}

fn split_off(slice: &str) -> Result<(&str, &str),&str> {
    let split = slice.split_once('|');
    if let Some((s1, s2)) = split {
        Ok((s1, s2))
    }
    else {
        Err(slice)
    }
}

fn match_short(ch: char, switch: &str) -> bool {
    let mut buf = [0u8; 4];
    let ch: &str = ch.encode_utf8(&mut buf);
    let split = split_off(switch);
    if let Ok((s1, s2)) = split {
        if s1 == ch {
            return true;
        }
        else if s2 == ch {
            return true;
        }
    }
    else if let Err(s) = split {
        if s == ch {
            return true;
        }
    }
    return false;
}

fn match_long(arg: &str, switch: &str) -> bool {
    if arg.chars().count() == 1 {
        return false;
    }

    let split = split_off(switch);
    if let Ok((s1, s2)) = split {
        if s1 == arg {
            return true;
        }
        else if s2 == arg {
            return true;
        }
    }
    else if let Err(s) = split {
        if s == arg {
            return true;
        }
    }
    return false;
}

fn handle_short_opt(args_iter: &mut Iter<&str>, arg: &str, opts: &mut [Opt]) -> Result<bool,Error> {
    let mut chars_iter = arg.chars().peekable();
    let mut parsed = false;
    while let Some(ch) = chars_iter.next() {
        let mut iter = opts.iter_mut();
        while let Some(opt) = iter.next() {
            if let Opt::Switch(switch, &mut ref mut b) = opt {
                if match_short(ch, switch) {
                    *b = true;
                    parsed = true;
                }
            }
            else if let Opt::Arg(switch, &mut ref mut s) = opt {
                if match_short(ch, switch) {
                    if chars_iter.peek().is_some() {
                        let a: String = chars_iter.collect();
                        *s = a;
                        // collect the rest as an argument here; if we don't return, we will
                        // possibly error when we didn't mean to.
                        return Ok(true);
                    }
                    else if let Some(a) = args_iter.next() {
                        *s = a.to_string();
                        parsed = true;
                    }
                    else {
                        return Err(Error::NeedArgument(format!("-{}", arg)));
                    }
                }
            }
            else if let Opt::SubArg(switch, func) = opt {
                if match_short(ch, switch) {
                    if let Some(a) = args_iter.next() {
                        func(arg, *a);
                        parsed = true;
                    }
                    else {
                        return Err(Error::NeedArgument(format!("-{}", arg)));
                    }
                }
            }
            else if let Opt::SubSwitch(switch, func) = opt {
                if match_short(ch, switch) {
                    func(arg);
                    parsed = true;
                }
            }
        }
    }

    if parsed {
        return Ok(true);
    }
    Err(Error::Unexpected(format!("-{}", arg)))
}

fn extract_equals(arg: &str) -> Option<(&str,&str)> {
    let split = arg.split_once('=');
    if let Some((arg, equals_value)) = split {
        Some((arg, equals_value))
    }
    else {
        None
    }
}

fn handle_long_opt(args_iter: &mut Iter<&str>, arg: &str, opts: &mut [Opt]) -> Result<bool,Error> {
    for opt in &mut *opts {
        if let Opt::Switch(switch, &mut ref mut b) = opt {
            if switch.chars().count() == 1 {
                continue;
            }

            if match_long(arg, switch) {
                *b = true;
                return Ok(true);
            }
        }
        else if let Opt::Arg(switch, &mut ref mut s) = opt {
            let (arg, equals_value) = extract_equals(arg).unwrap_or((arg, ""));

            if match_long(arg, switch) {
                if equals_value != "" {
                    *s = (equals_value).to_string();
                    return Ok(true);
                }
                else if let Some(a) = args_iter.next() {
                    *s = (*a).to_string();
                    return Ok(true);
                }
                else {
                    return Err(Error::NeedArgument(format!("--{}", arg)));
                }
            }
        }
        else if let Opt::SubArg(switch, func) = opt {
            let (arg, equals_value) = extract_equals(arg).unwrap_or((arg, ""));
            if match_long(arg, switch) {
                if equals_value != "" {
                    func(arg, equals_value);
                    return Ok(true);
                }
                else if let Some(a) = args_iter.next() {
                    func(arg, *a);
                    return Ok(true);
                }
                else {
                    return Err(Error::NeedArgument(format!("--{}", arg)));
                }
            }
        }
        else if let Opt::SubSwitch(switch, func) = opt {
            if match_long(arg, switch) {
                func(arg);
                return Ok(true);
            }
        }
    }
    Err(Error::Unexpected(format!("--{}", arg)))
}

fn handle_free_arg(arg: &str, opts: &mut [Opt]) -> Result<(),Error> {
    for opt in &mut *opts {
        if let Opt::Free(&mut ref mut vec) = opt {
            vec.push(arg.to_string());
            return Ok(());
        }
    }
    Err(Error::Unexpected(arg.to_string()))
}

pub fn get_options_env(opts: &mut [Opt]) -> Result<(),Error> {
    let env_args: Vec<String> = std::env::args().collect();
    get_options(opts, &env_args)
}

pub fn get_options(opts: &mut [Opt], args: &[String]) -> Result<(),Error> {
    let args: Vec<&str> = args.iter().map(|x| x.as_ref()).collect();
    get_options_str(opts, &args)
}

pub fn get_options_str(opts: &mut [Opt], args: &[&str]) -> Result<(),Error> {
    let mut iter = args.iter();
    while let Some(arg) = iter.next() {
        if *arg == "--" {
            while let Some(arg) = iter.next() {
                handle_free_arg(arg, opts)?;
                return Ok(());
            }
        }
        else if arg.starts_with("--") {
            let mut chars = arg.chars();
            chars.next();
            chars.next();
            let arg = chars.as_str();
            handle_long_opt(&mut iter, arg, opts)?;
        }
        else if arg.starts_with("-") {
            let mut chars = arg.chars();
            chars.next();
            let arg = chars.as_str();
            handle_short_opt(&mut iter, arg, opts)?;
        }
        else {
            handle_free_arg(arg, opts)?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::cell::RefCell;

    #[test]
    fn test_no_input() {
        let args = [];
        let result = get_options_str(&mut [], &args);
        assert!(result.is_ok());
    }

    #[test]
    fn test_short_switch() {
        let args = ["-a"];

        let mut a_opt = false;

        let result = get_options_str(&mut [
            Opt::Switch("a", &mut a_opt),
        ], &args);

        assert!(result.is_ok());
        assert!(a_opt);
    }

    #[test]
    fn test_short_switch_correct() {
        let args = ["-c"];

        let mut a_opt = false;
        let mut b_opt = false;

        let result = get_options_str(&mut [
            Opt::Switch("a", &mut a_opt),
            Opt::Switch("b", &mut b_opt),
        ], &args);

        assert!(!a_opt);
        assert!(!b_opt);
        assert!(result.is_err());
    }

    #[test]
    fn test_long_switch() {
        let args = ["--long-flag"];

        let mut long_flag = false;
        let mut another_flag = false;

        let result = get_options_str(&mut [
            Opt::Switch("long-flag", &mut long_flag),
            Opt::Switch("another-flag", &mut another_flag),
        ], &args);

        assert!(result.is_ok());
        assert!(long_flag);
        assert!(!another_flag);
    }

    #[test]
    fn test_long_and_short_switch() {
        let args = ["-a", "--long-flag"];

        let mut short_flag = false;
        let mut another_short_flag = false;
        let mut long_flag = false;
        let mut another_flag = false;

        let result = get_options_str(&mut [
            Opt::Switch("a", &mut short_flag),
            Opt::Switch("2", &mut another_short_flag),
            Opt::Switch("long-flag", &mut long_flag),
            Opt::Switch("another-flag", &mut another_flag),
        ], &args);

        assert!(result.is_ok());
        assert!(long_flag);
        assert!(!another_flag);
    }

    #[test]
    fn test_short_or_long_flag() {
        let args = ["--a-flag"];

        let mut flag = false;
        let mut flag2 = false;

        let result = get_options_str(&mut [
            Opt::Switch("a|a-flag", &mut flag),
            Opt::Switch("b|b-flag", &mut flag2),
        ], &args);

        assert!(result.is_ok());
        assert!(flag);
        assert!(!flag2);

        let args = ["-a"];

        let mut flag = false;
        let mut flag2 = false;

        let result = get_options_str(&mut [
            Opt::Switch("a|a-flag", &mut flag),
            Opt::Switch("b|b-flag", &mut flag2),
        ], &args);

        assert!(result.is_ok());
        assert!(flag);
        assert!(!flag2);
    }

    #[test]
    fn test_option_argument() {
        let args = ["-a", "foo bar"];

        let mut opt = String::new();

        let result = get_options_str(&mut [
            Opt::Arg("a|a-flag", &mut opt),
        ], &args);

        assert!(result.is_ok());
        assert_eq!(opt, "foo bar");

        let args = ["--a-flag", "foo bar"];

        let mut opt = String::new();

        let result = get_options_str(&mut [
            Opt::Arg("a|a-flag", &mut opt),
        ], &args);

        assert!(result.is_ok());
        assert_eq!(opt, "foo bar");
    }

    #[test]
    fn test_option_argument_take_last() {
        let args = ["-a", "foo bar", "--a-flag", "bar baz"];

        let mut opt = String::new();

        let result = get_options_str(&mut [
            Opt::Arg("a|a-flag", &mut opt),
        ], &args);

        assert!(result.is_ok());
        assert_eq!(opt, "bar baz");

        let args = ["--a-flag", "foo bar", "-a", "bar baz"];

        let mut opt = String::new();

        let result = get_options_str(&mut [
            Opt::Arg("a|a-flag", &mut opt),
        ], &args);

        assert!(result.is_ok());
        assert_eq!(opt, "bar baz");
    }

    #[test]
    fn test_option_argument_underflow() {
        let args = ["-a"];

        let mut opt = String::new();

        let result = get_options_str(&mut [
            Opt::Arg("a|a-flag", &mut opt),
        ], &args);

        assert_eq!(result, Err(Error::NeedArgument("-a".to_string())));
        assert_eq!(result.unwrap_err().to_string(), "need argument for '-a'");

        let args = ["--a-flag"];

        let mut opt = String::new();

        let result = get_options_str(&mut [
            Opt::Arg("a|a-flag", &mut opt),
        ], &args);

        assert_eq!(result, Err(Error::NeedArgument("--a-flag".to_string())));
    }

    #[test]
    fn test_callback() {
        let args = ["-f", "foo", "-f", "bar"];

        let opt = RefCell::new(String::new());

        let result = get_options_str(&mut [
            Opt::SubArg("flag|f", &|_arg, val| { opt.replace_with(|opt| format!("{}{}", opt, val)); }),
        ], &args);

        assert_eq!(result, Ok(()));
        assert_eq!(opt.into_inner(), "foobar");

        let args = ["--flag", "bar", "--flag", "foo"];

        let opt = RefCell::new(String::new());

        let result = get_options_str(&mut [
            Opt::SubArg("flag|f", &|_arg, val| { opt.replace_with(|opt| format!("{}{}", opt, val)); }),
        ], &args);

        assert_eq!(result, Ok(()));
        assert_eq!(opt.into_inner(), "barfoo");
    }

    #[test]
    fn test_callback_switch() {
        let args = ["-v", "-f", "-v", "-e", "-g"];

        let opt: RefCell<i64> = 0.into();

        let sub: &dyn for<'a> Fn(&'a str) = &|arg| {
            if arg == "f" {
                *opt.borrow_mut() += 2;
            }
            else if arg == "g" {
                *opt.borrow_mut() -= 3;
            }
        };

        let result = get_options_str(&mut [
            Opt::SubSwitch("verbose|v", &mut |_arg| {
                *opt.borrow_mut() += 1;
            }),
            Opt::SubSwitch("e", &mut |_arg| {
                *opt.borrow_mut() += 4;
            }),
            Opt::SubSwitch("f|g", sub),
        ], &args);

        assert_eq!(result, Ok(()));
        assert_eq!(opt, 5.into());
    }

    #[test]
    fn test_long_equals() {
        let args = ["--a-flag=boofar"];

        let mut opt = String::new();

        let result = get_options_str(&mut [
            Opt::Arg("a|a-flag", &mut opt),
        ], &args);

        assert_eq!(result, Ok(()));
        assert_eq!(opt, "boofar");
    }

    #[test]
    fn test_free_arguments() {
        let args = ["a", "bb", "ccc"];

        let mut free = vec![];

        let result = get_options_str(&mut [
            Opt::Free(&mut free),
        ], &args);

        assert_eq!(result, Ok(()));
        assert_eq!(free, vec!["a", "bb", "ccc"]);
    }

    #[test]
    fn test_double_dash_omit() {
        let args = ["--"];

        let mut free = vec![];

        let result = get_options_str(&mut [
            Opt::Free(&mut free),
        ], &args);

        assert_eq!(result, Ok(()));
        assert!(free.len() == 0);
    }

    #[test]
    fn test_double_dash_escape() {
        let args = ["--", "-f"];

        let mut flag = false;
        let mut free = vec![];

        let result = get_options_str(&mut [
            Opt::Switch("f|flag", &mut flag),
            Opt::Free(&mut free),
        ], &args);

        assert_eq!(result, Ok(()));
        assert!(!flag);
        assert!(free.len() == 1);
        assert_eq!(free, vec!("-f"));
    }

    #[test]
    fn test_switch_bundling() {
        let args = ["-ab"];

        let mut flag_a = false;
        let mut flag_b = false;

        let result = get_options_str(&mut [
            Opt::Switch("a", &mut flag_a),
            Opt::Switch("b", &mut flag_b),
        ], &args);

        assert!(result.is_ok());
        assert!(flag_a);
        assert!(flag_b);
    }

    #[test]
    fn test_switch_arg_bundling() {
        let args = ["-ab", "2"];

        let mut flag_a = false;
        let mut flag_b = String::new();

        let result = get_options_str(&mut [
            Opt::Switch("a", &mut flag_a),
            Opt::Arg("b", &mut flag_b),
        ], &args);

        assert!(result.is_ok());
        assert!(flag_a);
        assert_eq!(flag_b, "2");
    }

    #[test]
    fn test_arg_nospace() {
        let args = ["-a7"];

        let mut flag_a = String::new();

        let result = get_options_str(&mut [
            Opt::Arg("a", &mut flag_a),
        ], &args);

        assert!(result.is_ok());
        assert_eq!(flag_a, "7");
    }

    #[test]
    fn test_arg_nospace_longer() {
        let args = ["-aFooBarBaz"];

        let mut flag_a = String::new();

        let result = get_options_str(&mut [
            Opt::Arg("a", &mut flag_a),
        ], &args);

        assert!(result.is_ok());
        assert_eq!(flag_a, "FooBarBaz");
    }

    #[test]
    fn test_bundle_switch_arg_nospace() {
        let args = ["-ba8"];

        let mut flag_a = String::new();
        let mut flag_b = false;

        let result = get_options_str(&mut [
            Opt::Arg("a", &mut flag_a),
            Opt::Switch("b", &mut flag_b),
        ], &args);

        assert!(result.is_ok());
        assert_eq!(flag_a, "8");
        assert!(flag_b);
    }

    #[test]
    fn test_bundle_arg_switch_nospace() {
        let args = ["-ab8"];

        let mut flag_a = String::new();
        let mut flag_b = false;

        let result = get_options_str(&mut [
            Opt::Arg("a", &mut flag_a),
            Opt::Switch("b", &mut flag_b),
        ], &args);

        assert!(result.is_ok());
        assert_eq!(flag_a, "b8");
        assert!(!flag_b);
    }

    #[test]
    fn test_flag_invalid() {
        let args = ["-f"];

        let mut flag_a = false;

        let result = get_options_str(&mut [
            Opt::Switch("a", &mut flag_a),
        ], &args);

        assert_eq!(result, Err(Error::Unexpected("-f".to_string())));
        assert!(!flag_a);

        let args = ["--foo", "7"];

        let mut flag_a = false;
        let mut free = vec![];

        let result = get_options_str(&mut [
            Opt::Switch("a", &mut flag_a),
            Opt::Free(&mut free),
        ], &args);

        assert_eq!(result, Err(Error::Unexpected("--foo".to_string())));
        assert_eq!(result.unwrap_err().to_string(), "unexpected argument '--foo'");
        assert_eq!(free.len(), 0);
        assert!(!flag_a);
    }

    #[test]
    fn test_undeclared_free() {
        let args = ["foo", "bar", "baz"];

        let mut flag_a = false;

        let result = get_options_str(&mut [
            Opt::Switch("a", &mut flag_a),
        ], &args);

        assert_eq!(result, Err(Error::Unexpected("foo".to_string())));
        assert!(!flag_a);

        let args = ["--", "-7"];

        let mut flag_a = false;

        let result = get_options_str(&mut [
            Opt::Switch("a", &mut flag_a),
        ], &args);

        assert!(!flag_a);
        assert_eq!(result, Err(Error::Unexpected("-7".to_string())));
        assert_eq!(result.unwrap_err().to_string(), "unexpected argument '-7'");
    }

    #[test]
    fn test_callback_underfloaw() {
        let args = ["--foo", "7", "--bar"];

        let flag_a = RefCell::new(String::new());
        let flag_b = RefCell::new(String::new());

        let result = get_options_str(&mut [
            Opt::SubArg("foo", &mut |_arg, val| {
                flag_a.replace_with(|_| val.to_owned());
            }),
            Opt::SubArg("bar", &mut |_arg, val| {
                flag_b.replace_with(|_| val.to_owned());
            }),
        ], &args);

        assert_eq!(result, Err(Error::NeedArgument("--bar".to_string())));
        assert_eq!(flag_b.into_inner(), "");
    }

    #[test]
    fn test_mutually_exclusive() {
        let args = ["--foo", "--baz", "--bar", "--baz"];

        let command = RefCell::new(String::new());

        let sub: &dyn for<'a> Fn(&'a str) = &|arg| {
            let mut command = command.borrow_mut();
            *command = format!("command-{}", arg);
        };

        let result = get_options_str(&mut [
            Opt::SubSwitch("foo", sub),
            Opt::SubSwitch("bar", sub),
            Opt::SubSwitch("baz", sub),
        ], &args);

        assert_eq!(result, Ok(()));
        assert_eq!(command.into_inner(), "command-baz");
    }

    #[test]
    fn test_sledgehammer_example() {
        // pretend we've already done:
        // let args: Vec<String> = std::env::args().collect();
        let args: Vec<String> = vec!["/usr/local/bin/backup-profile", "--list", "-n", "--backup-dir=/mnt/usr1/backups", "--backup-dir", "/mnt/usr1/backups-test", "-v", "--", "-f.txt"].into_iter().map(|x| x.to_string()).collect();
        let prog = args[0].clone();
        let args: Vec<String> = args.into_iter().skip(1).collect();
        let usage =  || {
            let prog = std::path::Path::new(&prog).file_name().unwrap().to_string_lossy();
            println!("usage: {prog} [-hlcpnv] [-b BACKUP] [--] [TAR_ARGUMENTS]");
            println!("  backup firefox profiles on win32 from cygwin");
            println!("  -h,--help               display this usage");
            println!("  -b,--backup-dir BACKUP  output backup files to BACKUP (default '.')");
            println!("  -l,--list               list profiles and their paths");
            println!("  -n,--dry-run            perform dry run with commands printed");
            println!("  -p,--print              print Firefox configuration root");
            println!("  -v,--verbose            be noisier");
        };

        let mut backup_dir = String::new();
        let mut dry_run = false;
        let mut free_args = vec![];
        let mut list = false;
        let mut print = false;
        let mut verbose = false;

        let result = get_options(&mut [
            Opt::SubSwitch("help|h", &mut |_| { usage(); std::process::exit(0); }),
            Opt::Switch   ("list|l", &mut list),
            Opt::Switch   ("p|print", &mut print),
            Opt::Switch   ("dry-run|n", &mut dry_run),
            Opt::Arg      ("backup-dir|b", &mut backup_dir),
            Opt::Switch   ("verbose|v", &mut verbose),
            Opt::Free     (&mut free_args),
        ], &args);

        if let Err(ref err) = result {
            eprintln!("ERROR: parsing command line arguments: {err}");
            //std::process::exit(1); // don't do this in a test
        }

        // use the above options in a program...

        assert_eq!(backup_dir, "/mnt/usr1/backups-test");
        assert!(dry_run);
        assert_eq!(free_args, vec!["-f.txt"]);
        assert!(list);
        assert!(!print);
        assert!(verbose);

        assert_eq!(result, Ok(()));
    }
}

// perl example:
//
//     sub usage { say "usage: $0 [OPTIONS]... FILES..."; }
//     my @args = ();
//     sub add_arg {
//         push @args, @_;
//     }
//
//     my $options_file = ".optionsrc";
//     my $verbose;
//
//     GetOptions(
//         "h|help" => sub { usage; exit 0 },
//         "f|options-file" => \$options_file,
//         "v|verbose!" => \$verbose,
//         "<>" => \&add_arg)
//     or die "error parsing command line arguments: $@";
