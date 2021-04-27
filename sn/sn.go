/* Sneakernet */

package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"os/exec"
	"path"
	"strings"
)

const (
	subdir      = "transit"
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
	// FIXME: clean() is wrong for 'up' because it strips the directory.
	// For 'down' we may want that?  Or do we want to send the filename upstream
	// but then use the full path here?
	cleanArgs := clean(args[1:])
	if len(args) < 1 {
		fail()
	}
	switch args[0] {
	default:
		fail()
	case "help":
		usage()
	case "up":
		if len(cleanArgs) < 1 {
			fail()
		}
		runIt([]string{"scp", cleanArgs[0], userAndHost + ":" + subdir})
	case "down":
		if len(cleanArgs) < 1 {
			fail()
		}
		for _, arg := range cleanArgs {
			runIt([]string{"scp", userAndHost + ":" + subdir + "/" + arg, "."})
		}
	case "ls":
		runIt([]string{"ssh", userAndHost, "cd " + subdir + "; ls " + strings.Join(cleanArgs, " ")})
	case "rm":
		runIt([]string{"ssh", userAndHost, "cd " + subdir + "; rm " + strings.Join(cleanArgs, " ")})
	}
}

func clean(xs []string) []string {
	results := []string{}
	for _, x := range xs {
		probe := path.Base(path.Clean(x))
		if probe == "" {
			log.Fatalf("Bad file name: '%s'\n", x)
		}
		// The '/' here catches the root dir.
		if strings.IndexAny(probe, " \t\n\r/") != -1 {
			log.Fatalf("Bad file name: '%s'\n", x)
		}
		results = append(results, probe)
	}
	return results
}

func runIt(cmdLine []string) {
	if verbose {
		fmt.Printf("%v\n", cmdLine)
	}
	cmd := exec.Command(cmdLine[0], cmdLine[1:]...)
	output, err := cmd.Output()
	if err != nil {
		log.Fatal(err)
	}
	fmt.Print(string(output))
}

func fail() {
	usage()
	os.Exit(1)
}

func usage() {
	fmt.Print(`
Sneakernet - move files via transit point; inspect transit point

Usage:
  sn [-v] up filename ...
  sn [-v] down filename|pattern ...
  sn [-v] ls [[option] filename|pattern ...]
  sn [-v] rm [[option] filename|pattern ...]

Filenames may not contain blanks or path separators.
`)
}
