/* Create a binary file from textual byte values
 *
 * Synopsis
 *   writebin out-filename text ...
 *
 * Description
 *   The `text ...`, when joined by commas, comprises a space-or-comma
 *   separated sequence of decimal numbers representing byte values.  Write
 *   these bytes to the file named by `out-filename`.
 *
 *   The separator is one or more spaces, or a comma followed by zero or more
 *   spaces, where a space is anything in the \s class.
 *
 * Example
 *   $ writebin foo.txt 97 98 99 10
 *   $ cat foo.txt
 *   abc
 *
 * Bugs
 *   It should be possible to use "-" as the out-filename.
 *
 *   Hex and octal numbers are not allowed.
 */

package main

import (
  "io/ioutil"
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
    n, err := strconv.Atoi(bs); try(err)
    bytes = append(bytes, byte(n))
  }
  try(ioutil.WriteFile(outFilename, bytes, 0644))
}

func try(err error) {
  if err != nil {
    log.Panic(err)
  }
}
