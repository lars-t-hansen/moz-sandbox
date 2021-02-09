/* Create a binary file from textual byte values
 *
 * Synopsis
 *   writebin out-filename text ...
 *
 * Description
 *   The `text ...`, when joined by commas, comprises a space-or-comma
 *   separated sequence of unsigned numbers representing byte values.  Write
 *   these bytes to the file named by `out-filename`.  If `out-filename`
 *   is `-` then the bytes are written to stdout.
 *
 *   The separator is one or more spaces, or a comma followed by zero or more
 *   spaces, where a space is anything in the \s class.
 *
 *   Numbers can be binary (0b...), octal (0...) or hex (0x...).
 *
 * Example
 *   $ writebin foo.txt 97 98 99 0x64 0b1010
 *   $ cat foo.txt
 *   abcd
 *   $
 *
 * Bugs
 *   It should be possible to read from stdin, probably this should be
 *   the default if there are no text ... arguments, as for cat, and
 *   maybe in this case the output could default to stdout.
 */

package main

import (
  "log"
  "os"
  "regexp"
  "strconv"
  "strings"
)

func main() {
  if len(os.Args) < 3 {
    log.Panic("Usage: writebin out-filename text ...")
  }
  outFilename := os.Args[1]

  re, err := regexp.Compile(`\s+|,\s*`); try(err)
  bytes := []byte {}
  for _, bs := range re.Split(strings.Join(os.Args[2:],","), -1) {
    n, err := strconv.ParseUint(bs, 0, 8); try(err)
    bytes = append(bytes, byte(n))
  }

  var outfile *os.File
  if outFilename == "-" {
    outfile = os.Stdout
  } else {
    outfile, err = os.OpenFile(outFilename, os.O_WRONLY|os.O_CREATE, 0644); try(err)
    defer outfile.Close()
  }
  _, err = outfile.Write(bytes); try(err)
}

func try(err error) {
  if err != nil {
    log.Panic(err)
  }
}
