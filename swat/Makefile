# Run .wast.js
JS=~/bin/jsshell
JSOPTS=--wasm-gc

# Compile .swat to .js and other files
SWAT=./swat

# Tests that should pass
OBJS=fib.js while.js loop.js multi-modules.js global.js syntax.js bool.js \
	unop.js binop.js misc.js conversion.js class.js evalexpr.js wabbit.js \
	number.js lex.js anyref.js string.js vector.js

# Tests that should fail
FAILOBJS=binop-mismatch.fail.js

.PHONY: test test-success test-failure clean snake

swat swat.sch.slfasl: swat.sch
	compile-larceny swat.sch -o swat

test: test-success test-failure snake 

test-success: swat Makefile $(OBJS)
	@for i in $(OBJS); do echo $(JS) $(JSOPTS) $$i; $(JS) $(JSOPTS) $$i; done

test-failure: swat Makefile $(FAILOBJS)

clean:
	rm -f $(OBJS) $(FAILOBJS) swat swat.sch.slfasl snake.metawasm.js

snake: snake.js snake.wasm

snake.js snake.metawasm.js: swat binarize.sch Makefile snake.swat
	$(SWAT) --js+wasm snake.swat

$(OBJS) $(FAILOBJS): swat Makefile

%.js : %.swat
	$(SWAT) --js $<

%.fail.js : %.swat
	$(SWAT) --fail $<
	touch $@

%.wasm : %.metawasm.js
	$(JS) $(JSOPTS) $< > TEMP
	./binarize TEMP $@
	@rm -f TEMP

%.bytes.js : %.metabytes.js
	$(JS) $(JSOPTS) $< > $@
