JS=~/bin/js
JSOPTS=--wasm-gc

SWAT=~/moz/sandbox/swat/swat
SWATOPTS=--js

COMPILE=~/moz/sandbox/swat/compile

# Tests that should pass

OBJS=fib.wast.js while.wast.js loop.wast.js multi-modules.wast.js global.wast.js \
	syntax.wast.js bool.wast.js unop.wast.js binop.wast.js misc.wast.js conversion.wast.js \
	class.wast.js evalexpr.wast.js wabbit.wast.js

# Tests that should fail

FAILOBJS=binop-mismatch.fwast.js

.PHONY: test clean test-success test-failure

test: test-success test-failure

test-success: $(OBJS)
	@for i in $^; do echo $(JS) $(JSOPTS) $$i; $(JS) $(JSOPTS) $$i; done

test-failure: $(FAILOBJS)
	@for i in $^; do echo $(JS) $(JSOPTS) $$i; done

clean:
	rm -f $(OBJS) $(FAILOBJS)

%.wast.js : %.swat swat.fasl Makefile
	$(SWAT) $(SWATOPTS) $<

%.fwast.js : %.swat swat.fasl Makefile
	$(SWAT) $(SWATOPTS) --fail $<
	touch $@

%.fasl : %.sch
	$(COMPILE) $<
