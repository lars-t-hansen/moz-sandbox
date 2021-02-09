/* Usage: writebin out-filename text ...
 *
 * The `text ...`, when joined by spaces, comprises a space-or-comma separated
 * sequence of decimal digits representing byte values.  Write these bytes
 * to the file named by `out-filename`.
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
  text := strings.Join(os.Args[2:],",")
  re, err := regexp.Compile(`[, ]`)
  if err != nil {
    log.Panic(err)
  }
  bytes := []byte {}
  for _, bs := range re.Split(text, -1) {
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
/*
  data := []byte{
0,97,115,109,1,0,0,0,1,4,1,96,0,0,3,2,1,0,5,4,1,1,4,4,7,8,1,4,109,97,105,110,0,0,10,13,1,11,0,65,64,65,65,254,0,2,1,0,11,0,14,4,110,97,109,101,1,7,1,0,4,109,97,105,110,
  }
  err := ioutil.WriteFile("test.wasm", data, 0644)
  if err != nil {
    log.Fatal(err)
  }
  */
}
