// Word count
//
// Count lines, words, and characters for files provided on the command line.

use std::os;
use std::io::{BufferedReader,File};

fn main() {
  for argument in os::args().iter().skip(1) {
    let path = Path::new(argument.clone());
    let mut file = BufferedReader::new(File::open(&path));
    let mut l = 0u;
    let mut w = 0u;
    let mut c = 0u;
    for line in file.lines() {
      let txt = line.unwrap();
      l += 1u;
      c += txt.len() + 1u; // 1u extra for the terminator
      let mut inside = false;
      for ch in txt.as_slice().chars() {
        if ch.is_alphabetic() {
          if !inside {
            w += 1u;
            inside = true;
          }
        }
        else {
          inside = false;
        }
      }
    }
    print!("Lines: {}  Words: {}  Chars: {}   {}\n", l, w, c, argument.clone());
  }
}
