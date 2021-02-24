/* Usage: make-nonunified rootdir-name
 *
 * Scan the directory tree rooted at rootdir-name and rewrite UNIFIED_SOURCES to SOURCES
 * in every moz.build file in the tree.
 */
package main

import (
  "flag"
  "fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strings"
)

func main() {
	help := false
	flag.BoolVar(&help, "h", false, "Print help")
	flag.BoolVar(&help, "help", false, "Print help")
	flag.Parse()
	if help {
		usage()
		return
	}

	args := flag.Args()
	if len(args) != 1 {
		usage()
		os.Exit(1)
	}
	rootDirname := args[0]
	err := filepath.Walk(rootDirname,
		func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}
			if info.Name() == "moz.build" {
				input, err := ioutil.ReadFile(path)
				if err != nil {
					return err
				}
				output := strings.ReplaceAll(string(input), "UNIFIED_SOURCES", "SOURCES")
				err = ioutil.WriteFile(path, []byte(output), info.Mode())
				if err != nil {
					return err
				}
			}
			return nil
		})
	if err != nil {
		log.Panic(err)
	}
}

func usage() {
	fmt.Println(`
Usage
  make-nonunified rootdir-name

Description
  Rewrites all UNIFIED_SOURCES as SOURCES in the moz.build files
  of the tree rooted at "rootdir-name"
`)
}
