/* Sneakernet */

// TODO: rm command?
// TODO: We sort of want to launder file names and patterns, to avoid accidents...

package main

import (
	"flag"
	"fmt"
	"os"
  "os/exec"
  "strings"
	S "github.com/lars-t-hansen/scripting"
)

const (
	subdir = "transit"
	userAndHost = "lth@axis-of-eval.org"
)

var verbose bool

func main() {
	help := false
	flag.BoolVar(&help, "h", false, "Print help")
	flag.BoolVar(&help, "help", false, "Print help")
	flag.BoolVar(&verbose, "v", false, "Be verbose")
	flag.BoolVar(&verbose, "verbose", false, "Be verbose")
	flag.Parse()
	if help {
		usage()
		return
	}
	
	args := flag.Args()
	if len(args) < 1 {
		fail()
	}
	switch args[0] {
	default:
		fail()
	case "help":
		usage()
	case "up":
		if len(args) < 2 {
			fail()
		}
		runIt([]string {"scp", args[1], userAndHost + ":" + subdir})
	case "down":
		if len(args) < 2 {
			fail()
		}
		for _, arg := range args[1:] {
			runIt([]string {"scp", userAndHost + ":" + subdir + "/" + arg, "."})
		}
	case "ls":
    runIt([]string {"ssh", userAndHost, "cd " + subdir + "; ls " + strings.Join(args[1:], " ")})
	}
}

func runIt(cmdLine []string) {
	if verbose {
		fmt.Printf("%v\n", cmdLine)
	}
	cmd := exec.Command(cmdLine[0], cmdLine[1:]...)
	output, err := cmd.Output()
	S.Try(err)
	fmt.Print(string(output))
}

func fail() {
	usage()
	os.Exit(1)
}

func usage() {
	fmt.Print(`
Sneakernet - move files between local machine and transit point

Usage:
  sn [-v] up filename ...
  sn [-v] down filename|pattern ...
  sn [-v] ls [[option] filename|pattern ...]
`)
}
