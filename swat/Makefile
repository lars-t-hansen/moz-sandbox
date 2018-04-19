JS=~/m-i/js/src/build-release/dist/bin/js
SWAT=~/moz/sandbox/swat/swat

OBJS=fib.wast.js let.wast.js while.wast.js loop.wast.js

test: $(OBJS)
	$(JS) fib.wast.js
	$(JS) let.wast.js
	$(JS) while.wast.js
	$(JS) loop.wast.js

clean:
	rm -f $(OBJS)

fib.wast.js: fib.swat swat.sch
	$(SWAT) --js fib.swat

let.wast.js: let.swat swat.sch
	$(SWAT) --js let.swat

while.wast.js: while.swat swat.sch
	$(SWAT) --js while.swat

loop.wast.js: loop.swat swat.sch
	$(SWAT) --js loop.swat
