JS=~/m-i/js/src/build-debug/dist/bin/js
SWAT=~/moz/sandbox/swat/swat

OBJS=fib.wast.js while.wast.js loop.wast.js multi-modules.wast.js global.wast.js \
	syntax.wast.js bool.wast.js unop.wast.js binop.wast.js misc.wast.js conversion.wast.js \
	class.wast.js evalexpr.wast.js wabbit.wast.js

FAILOBJS=binop-mismatch.fwast.js

.PHONY: test clean test-success test-failure

test: test-success test-failure

test-success: $(OBJS)
	@for i in $^; do echo $(JS) $$i; $(JS) --wasm-gc $$i; done

test-failure: $(FAILOBJS)
	@for i in $^; do echo $(JS) $$i; done

clean:
	rm -f $(OBJS) $(FAILOBJS)

%.wast.js : %.swat swat.sch
	$(SWAT) --js $<

%.fwast.js : %.swat swat.sch
	$(SWAT) --fail $<
	touch $@

