#!/usr/bin/python

import sys
import re

def process(filename):
    f = open(filename, 'r')
    inside = False
    nesting = 0
    lineno = 0
    # FIXME: does not handle elif .. defined(DEBUG), but not sure
    # if this is a problem.
    for line in f:
        lineno = lineno + 1
        if not inside:
            if re.match(r"\s*#\s*ifdef\s+DEBUG", line) or re.match(r"\s*#\s*if\s+.*defined\s*\(?\s*DEBUG[^a-zA-Z0-9_]", line):
                inside = True
                nesting = 1
                continue
        else:
            if re.match(r"\s*#\s*endif", line):
                nesting = nesting-1
                if nesting == 0:
                    inside = False
                continue
            if re.match(r"\s*#\s*if(def)?\s+", line):
                nesting = nesting+1
                continue
            if re.match(r".*JitSpew_.*", line):
                sys.stdout.write(filename + ":" + str(lineno) + ": " + line)
    f.close()

for filename in sys.argv[1:]:
    process(filename)

