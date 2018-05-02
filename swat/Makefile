# Run .wast.js
JS=~/bin/jsshell
JSOPTS=--wasm-gc

# Compile .swat to .wast.js
SWAT=~/moz/sandbox/swat/swat
SWATOPTS=--js

# Compile .sch to .fasl
COMPILE=~/moz/sandbox/swat/compile

# Compiler objects
FASLS=swat.fasl pp.fasl

# Tests that should pass
OBJS=fib.wast.js while.wast.js loop.wast.js multi-modules.wast.js global.wast.js \
	syntax.wast.js bool.wast.js unop.wast.js binop.wast.js misc.wast.js conversion.wast.js \
	class.wast.js evalexpr.wast.js wabbit.wast.js number.wast.js

# Tests that should fail
FAILOBJS=binop-mismatch.fwast.js

.PHONY: obj test clean test-success test-failure

obj: $(FASLS)

test: obj test-success test-failure

test-success: $(OBJS)
	@for i in $^; do echo $(JS) $(JSOPTS) $$i; $(JS) $(JSOPTS) $$i; done

test-failure: $(FAILOBJS)

clean:
	rm -f $(OBJS) $(FAILOBJS) $(FASLS)

%.wast.js : %.swat obj Makefile
	$(SWAT) $(SWATOPTS) $<

%.fwast.js : %.swat obj Makefile
	$(SWAT) $(SWATOPTS) --fail $<
	touch $@

%.fasl : %.sch
	$(COMPILE) $<
