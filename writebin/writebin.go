/* Create a binary file from textual byte values
 *
 * Synopsis
 *   writebin out-filename text ...
 *   writebin out-filename
 *   writebin
 *
 * Description
 *   The `text ...`, when joined by commas, comprises a space-or-comma
 *   separated sequence of unsigned numbers representing byte values.  Write
 *   these bytes to the file named by `out-filename`.  If `out-filename`
 *   is `-` then the bytes are written to stdout.
 *
 *   If there are no `text ...` arguments then the input is read from stdin.
 *   In this case, if there is no `out-filename` then the output is written
 *   to stdout.
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
	outFilename := "-"
	if len(os.Args) > 1 {
		outFilename = os.Args[1]
	}

	var input string
	if len(os.Args) < 3 {
		bytes, err := ioutil.ReadAll(os.Stdin)
		try(err)
		input = string(bytes)
	} else {
		input = strings.Join(os.Args[2:], ",")
	}

	re, err := regexp.Compile(`\s+|,\s*`)
	try(err)
	bytes := []byte{}
	for _, bs := range re.Split(strings.Trim(input, " \t\n\r"), -1) {
		n, err := strconv.ParseUint(bs, 0, 8)
		try(err)
		bytes = append(bytes, byte(n))
	}

	outfile := os.Stdout
	if outFilename != "-" {
		outfile, err = os.OpenFile(outFilename, os.O_WRONLY|os.O_CREATE, 0644)
		try(err)
		defer outfile.Close()
	}
	_, err = outfile.Write(bytes)
	try(err)
}

func try(err error) {
	if err != nil {
		log.Panic(err)
	}
}
