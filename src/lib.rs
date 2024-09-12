/// getoptions-long - library to parse command line inspired by perl's Getopt::Long
/// Copyright (C) 2024  Ira Peach
///
/// This program is free software: you can redistribute it and/or modify
/// it under the terms of the GNU Affero General Public License as published by
/// the Free Software Foundation, either version 3 of the License, or
/// (at your option) any later version.
///
/// This program is distributed in the hope that it will be useful,
/// but WITHOUT ANY WARRANTY; without even the implied warranty of
/// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/// GNU Affero General Public License for more details.
///
/// You should have received a copy of the GNU Affero General Public License
/// along with this program.  If not, see <https://www.gnu.org/licenses/>.

use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::slice::Iter;

pub enum Opt<'a> {
    Switch(&'a str, &'a mut bool),
    Arg(&'a str, &'a mut String),
    SubArg(&'a str, &'a mut dyn FnMut(&str,&str)),
    SubSwitch(&'a str, &'a mut dyn FnMut(&str)),
    Free(&'a mut Vec<String>),
    //Opt(&'a str, &'a mut str),
}

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

        let mut opt = String::new();

        let result = get_options_str(&mut [
            Opt::SubArg("flag|f", &mut |_arg, val| { opt = format!("{}{}", opt, val); }),
        ], &args);

        assert_eq!(result, Ok(()));
        assert_eq!(opt, "foobar");

        let args = ["--flag", "bar", "--flag", "foo"];

        let mut opt = String::new();

        let result = get_options_str(&mut [
            Opt::SubArg("flag|f", &mut |_arg, val| { opt = format!("{}{}", opt, val); }),
        ], &args);

        assert_eq!(result, Ok(()));
        assert_eq!(opt, "barfoo");
    }

    #[test]
    fn test_callback_switch() {
        let args = ["-v", "-f", "-v", "-e", "-g"];

        use std::cell::RefCell;

        let opt: RefCell<i64> = 0.into();

        let sub: &mut dyn for<'a> FnMut(&'a str) = &mut |arg| {
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

        let mut flag_a = String::new();
        let mut flag_b = String::new();

        let result = get_options_str(&mut [
            Opt::SubArg("foo", &mut |_arg, val| {
                flag_a = val.to_owned();
            }),
            Opt::SubArg("bar", &mut |_arg, val| {
                flag_b = val.to_owned();
            }),
        ], &args);

        assert_eq!(result, Err(Error::NeedArgument("--bar".to_string())));
        assert_eq!(flag_b, "");
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
