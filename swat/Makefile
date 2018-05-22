# Run .wast.js
JS=~/bin/jsshell
JSOPTS=--wasm-gc

# Compile .swat to .js and other files
SWAT=./swat

# Compile .sch to .fasl
COMPILE=./compile

# Compiler objects
FASLS=swat.sch.slfasl

# Tests that should pass
OBJS=fib.js while.js loop.js multi-modules.js global.js syntax.js bool.js \
	unop.js binop.js misc.js conversion.js class.js evalexpr.js wabbit.js \
	number.js lex.js anyref.js string.js vector.js

# Tests that should fail
FAILOBJS=binop-mismatch.fail.js

.PHONY: fasl test test-success test-failure clean snake

fasl: $(FASLS)

test: test-success test-failure snake

test-success: $(OBJS)
	@for i in $^; do echo $(JS) $(JSOPTS) $$i; $(JS) $(JSOPTS) $$i; done

test-failure: $(FAILOBJS)

clean:
	rm -f $(OBJS) $(FAILOBJS) $(FASLS) snake.metawasm.js

snake: snake.js snake.wasm

snake.js snake.metawasm.js: snake.swat Makefile $(FASLS)
	$(SWAT) --js+wasm $<

$(OBJS) $(FAILOBJS): Makefile $(FASLS)

%.js : %.swat
	$(SWAT) --js $<

%.fail.js : %.swat
	$(SWAT) --fail $<
	touch $@

%.sch.slfasl : %.sch
	compile-larceny $<

%.wasm : %.metawasm.js
	$(JS) $(JSOPTS) $< > TEMP
	./binarize TEMP $@
	@rm -f TEMP
