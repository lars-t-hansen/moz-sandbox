/* Usage: make-nonunified rootdir-name
 *
 * Scan the directory tree rooted at rootdir-name and rewrite UNIFIED_SOURCES to SOURCES
 * in every moz.build file in the tree.
 */
package main

import (
 "io/ioutil"
 "log"
 "os"
 "path/filepath"
 "strings"
)

func main() {
  if len(os.Args) != 2 {
    log.Fatal("Usage: make-nonunified rootdir-name")
  }
  rootDirname := os.Args[1]
  err := filepath.Walk(rootDirname, func(path string, info os.FileInfo, err error) error {
    if err != nil { return err }
    if info.Name() == "moz.build" {
      input, err := ioutil.ReadFile(path)
      if err != nil { return err }
      output := strings.ReplaceAll(string(input), "UNIFIED_SOURCES", "SOURCES")
      err = ioutil.WriteFile(path, []byte(output), info.Mode())
      if err != nil { return err }
    }
    return nil
  })
  if err != nil {
    log.Fatal(err)
  }
}