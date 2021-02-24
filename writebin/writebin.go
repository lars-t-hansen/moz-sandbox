/* Create a binary file from textual byte values. */
package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strconv"
	"strings"

	S "github.com/lars-t-hansen/scripting"
)

const helpText = `		
Create a binary file from textual byte values.

Usage
   writebin -h
   writebin [out-filename [text ...]]

Description
  The "text ..." arguments, when joined by commas, comprises a space-or-comma
  separated sequence of unsigned numbers representing byte values.  The program
  writes these bytes to the file named by "out-filename".  If "out-filename" is
  "-" then the bytes are written to stdout.

  If there are no "text ..." arguments then the input is read from stdin.  In
  this case, if there is no "out-filename" then the output is written to stdout.

  Numbers can be binary (0b...), octal (0...) or hex (0x...).

Example
  $ writebin foo.txt 97 98 99 0x64 0b1010
  $ cat foo.txt
  abcd
  $
`

func main() {
	help := false
	flag.BoolVar(&help, "h", false, "Print help")
	flag.BoolVar(&help, "help", false, "Print help")
	flag.Parse()
	if help {
		fmt.Print(helpText)
		return
	}

	var err error
	args := flag.Args()
	outFilename := "-"
	if len(args) > 0 {
		outFilename = args[0]
	}

	var input string
	if len(args) < 2 {
		bytes, err := ioutil.ReadAll(os.Stdin)
		S.Try(err)
		input = string(bytes)
	} else {
		input = strings.Join(args[1:], ",")
	}

	re := regexp.MustCompile(`\s+|,\s*`)
	bytes := []byte{}
	for _, bs := range re.Split(strings.Trim(input, " \t\n\r"), -1) {
		n, err := strconv.ParseUint(bs, 0, 8)
		S.Try(err)
		bytes = append(bytes, byte(n))
	}

	outfile := os.Stdout
	if outFilename != "-" {
		outfile, err = os.Create(outFilename)
		S.Try(err)
		defer outfile.Close()
	}
	_, err = outfile.Write(bytes)
	S.Try(err)
}
