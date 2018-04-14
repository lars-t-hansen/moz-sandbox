JS=~/m-i/js/src/build-release/dist/bin/js
SWAT=~/moz/sandbox/swat/swat

test: fib.wast.js let.wast.js
	$(JS) fib.wast.js
	$(JS) let.wast.js

fib.wast.js: fib.swat
	$(SWAT) --js fib.swat

let.wast.js: let.swat
	$(SWAT) --js let.swat

