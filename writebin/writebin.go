/* Usage: writebin out-filename text ...
 *
 * The `text ...`, when joined by spaces, comprises a space-or-comma separated
 * sequence of decimal digits representing byte values.  Write these bytes
 * to the file named by `out-filename`.
 *
 * The separator is one or more spaces, or a comma followed by zero or more
 * spaces, where a space is anything in the \s class.
 */

package main

import (
  "io/ioutil"
  "log"
  "os"
  "strconv"
  "strings"
  "regexp"
  )

func main() {
  if len(os.Args) < 3 {
    log.Panic("Usage: writebin out-filename text ...")
  }
  outFilename := os.Args[1]
  re, err := regexp.Compile(`\s+|,\s*`)
  if err != nil {
    log.Panic(err)
  }
  bytes := []byte {}
  for _, bs := range re.Split(strings.Join(os.Args[2:],","), -1) {
    n, err := strconv.Atoi(bs)
    if err != nil {
      log.Panic(err)
    }
    bytes = append(bytes, byte(n))
  }
  err = ioutil.WriteFile(outFilename, bytes, 0644)
  if err != nil {
    log.Panic(err)
  }
}
