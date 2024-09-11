#![allow(unused)]
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

pub enum Opt<'a> {
    Switch(&'a str, &'a mut bool),
    Arg(&'a str, &'a mut String),
    SubArg(&'a str, &'a mut dyn FnMut(&str,&str)),
    SubSwitch(&'a str, &'a mut dyn FnMut(&str)),
    //Opt(&'a str, &'a mut str),
}

use std::fmt::Debug;
use std::fmt::Formatter;

impl Debug for Opt<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Opt::Switch(x, b) => f.debug_tuple("Opt::Switch").field(x).field(b).finish(),
            Opt::Arg(x, s) => f.debug_tuple("Opt::Arg").field(x).field(s).finish(),
            Opt::SubArg(x, _) => f.debug_tuple("Opt::SubArg").field(x).field(&"Fn").finish(),
            Opt::SubSwitch(x, _) => f.debug_tuple("Opt::SubSwitch").field(x).field(&"Fn").finish(),
        }
    }
}

#[derive(Clone,Debug,Eq,PartialEq)]
pub enum Error {
    NeedArgument(String),
}

use std::env;
use std::slice::Iter;

fn split_off(slice: &str) -> Result<(&str, &str),&str> {
    let split = slice.split_once('|');
    if let Some((s1, s2)) = split {
        Ok((s1, s2))
    }
    else {
        Err(slice)
    }
}

fn match_short(arg: &str, switch: &str) -> bool {
    if arg.chars().count() != 1 {
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
    let mut iter = opts.iter_mut();
    while let Some(opt) = iter.next() {
        if let Opt::Switch(switch, &mut ref mut b) = opt {
            if match_short(arg, switch) {
                *b = true;
                return Ok(true);
            }
        }
        else if let Opt::Arg(switch, &mut ref mut s) = opt {
            if match_short(arg, switch) {
                if let Some(a) = args_iter.next() {
                    *s = a.to_string();
                    return Ok(true);
                }
                else {
                    return Err(Error::NeedArgument(format!("-{}", arg)));
                }
            }
        }
        else if let Opt::SubArg(switch, func) = opt {
            if match_short(arg, switch) {
                if let Some(a) = args_iter.next() {
                    func(arg, *a);
                }
                else {
                    return Err(Error::NeedArgument(format!("-{}", arg)));
                }
            }
        }
        else if let Opt::SubSwitch(switch, func) = opt {
            if match_short(arg, switch) {
                func(arg);
                return Ok(true);
            }
        }
    }
    return Ok(false);
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
    return Ok(false);
}

pub fn get_options(opts: &mut [Opt], args: &[&str]) -> Result<(),Error> {
    let mut iter = args.iter();
    while let Some(arg) = iter.next() {
        if arg.starts_with("--") {
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
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_input() {
        let args = [];
        let result = get_options(&mut [], &args);
        assert!(result.is_ok());
    }

    #[test]
    fn test_short_switch() {
        let args = ["-a"];

        let mut a_opt = false;

        get_options(&mut [
            Opt::Switch("a", &mut a_opt),
        ], &args);

        assert!(a_opt);
    }

    #[test]
    fn test_short_switch_correct() {
        let args = ["-c"];

        let mut a_opt = false;
        let mut b_opt = false;

        get_options(&mut [
            Opt::Switch("a", &mut a_opt),
            Opt::Switch("b", &mut b_opt),
        ], &args);

        assert!(!a_opt);
        assert!(!b_opt);
    }

    #[test]
    fn test_long_switch() {
        let args = ["--long-flag"];

        let mut long_flag = false;
        let mut another_flag = false;

        get_options(&mut [
            Opt::Switch("long-flag", &mut long_flag),
            Opt::Switch("another-flag", &mut another_flag),
        ], &args);

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

        get_options(&mut [
            Opt::Switch("a", &mut short_flag),
            Opt::Switch("2", &mut another_short_flag),
            Opt::Switch("long-flag", &mut long_flag),
            Opt::Switch("another-flag", &mut another_flag),
        ], &args);

        assert!(long_flag);
        assert!(!another_flag);
    }

    #[test]
    fn test_short_or_long_flag() {
        let args = ["--a-flag"];

        let mut flag = false;
        let mut flag2 = false;

        get_options(&mut [
            Opt::Switch("a|a-flag", &mut flag),
            Opt::Switch("b|b-flag", &mut flag2),
        ], &args);

        assert!(flag);
        assert!(!flag2);

        let args = ["-a"];

        let mut flag = false;
        let mut flag2 = false;

        get_options(&mut [
            Opt::Switch("a|a-flag", &mut flag),
            Opt::Switch("b|b-flag", &mut flag2),
        ], &args);

        assert!(flag);
        assert!(!flag2);
    }

    #[test]
    fn test_option_argument() {
        let args = ["-a", "foo bar"];

        let mut opt = String::new();

        get_options(&mut [
            Opt::Arg("a|a-flag", &mut opt),
        ], &args);

        assert_eq!(opt, "foo bar");

        let args = ["--a-flag", "foo bar"];

        let mut opt = String::new();

        get_options(&mut [
            Opt::Arg("a|a-flag", &mut opt),
        ], &args);

        assert_eq!(opt, "foo bar");
    }

    #[test]
    fn test_option_argument_take_last() {
        let args = ["-a", "foo bar", "--a-flag", "bar baz"];

        let mut opt = String::new();

        get_options(&mut [
            Opt::Arg("a|a-flag", &mut opt),
        ], &args);

        assert_eq!(opt, "bar baz");

        let args = ["--a-flag", "foo bar", "-a", "bar baz"];

        let mut opt = String::new();

        get_options(&mut [
            Opt::Arg("a|a-flag", &mut opt),
        ], &args);

        assert_eq!(opt, "bar baz");
    }

    #[test]
    fn test_option_argument_underflow() {
        let args = ["-a"];

        let mut opt = String::new();

        let result = get_options(&mut [
            Opt::Arg("a|a-flag", &mut opt),
        ], &args);

        assert_eq!(result, Err(Error::NeedArgument("-a".to_string())));

        let args = ["--a-flag"];

        let mut opt = String::new();

        let result = get_options(&mut [
            Opt::Arg("a|a-flag", &mut opt),
        ], &args);

        assert_eq!(result, Err(Error::NeedArgument("--a-flag".to_string())));
    }

    #[test]
    fn test_callback() {
        let args = ["-f", "foo", "-f", "bar"];

        let mut opt = String::new();

        let result = get_options(&mut [
            Opt::SubArg("flag|f", &mut |arg, val| { opt = format!("{}{}", opt, val); }),
        ], &args);

        assert_eq!(result, Ok(()));
        assert_eq!(opt, "foobar");

        let args = ["--flag", "bar", "--flag", "foo"];

        let mut opt = String::new();

        let result = get_options(&mut [
            Opt::SubArg("flag|f", &mut |arg, val| { opt = format!("{}{}", opt, val); }),
        ], &args);

        assert_eq!(result, Ok(()));
        assert_eq!(opt, "barfoo");
    }

    #[test]
    fn test_callback_switch() {
        let args = ["-v", "-f", "-v", "-e", "-g"];

        use std::rc::Rc;
        use std::cell::Cell;
        use std::cell::RefCell;

        let opt: RefCell<i64> = 0.into();

        let mut sub: &mut dyn for<'a> FnMut(&'a str) = &mut |arg| {
            if arg == "f" {
                *opt.borrow_mut() += 2;
            }
            else if arg == "g" {
                *opt.borrow_mut() -= 3;
            }
        };

        let result = get_options(&mut [
            Opt::SubSwitch("verbose|v", &mut |arg| {
                *opt.borrow_mut() += 1;
            }),
            Opt::SubSwitch("e", &mut |arg| {
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

        let result = get_options(&mut [
            Opt::Arg("a|a-flag", &mut opt),
        ], &args);

        assert_eq!(result, Ok(()));
        assert_eq!(opt, "boofar");
    }

    //#[test]
    //fn test_short_switch_correct() {
    //    let args = ["-c"];

    //    let mut a_opt = false;
    //    let mut b_opt = false;

    //    get_options(&mut [
    //        Opt::Switch("a", &mut a_opt),
    //        Opt::Switch("b", &mut b_opt),
    //    ], &args);

    //    assert!(!a_opt);
    //    assert!(!b_opt);
    //}
}

//sub usage { say "usage: $0 [OPTIONS]... FILES..."; }
//my @args = ();
//sub add_arg {
//    push @args, @_;
//}
//
//my $options_file = ".optionsrc";
//my $verbose;
//
//GetOptions(
//    "h|help" => sub { usage; exit 0 },
//    "f|options-file" => \$options_file,
//    "v|verbose!" => \$verbose,
//    "<>" => \&add_arg)
//or die "error parsing command line arguments: $@";
