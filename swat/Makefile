JS=~/m-i/js/src/build-debug/dist/bin/js
SWAT=~/moz/sandbox/swat/swat

OBJS=fib.wast.js while.wast.js loop.wast.js multi-modules.wast.js global.wast.js \
	syntax.wast.js bool.wast.js unop.wast.js binop.wast.js misc.wast.js conversion.wast.js \
	class.wast.js

.PHONY: test clean

test: $(OBJS)
	@for i in $^; do echo $(JS) $$i; $(JS) --wasm-gc $$i; done

clean:
	rm -f $(OBJS)

%.wast.js : %.swat swat.sch
	$(SWAT) --js $<
