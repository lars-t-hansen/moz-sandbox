# Run .wast.js
JS=~/bin/jsshell
JSOPTS=--wasm-gc

# Compile .swat to .js and other files
SWAT=./swat

# Compile .sch to .fasl
COMPILE=./compile

# Compiler objects
FASLS=swat.fasl pp.fasl

# Tests that should pass
OBJS=fib.js while.js loop.js multi-modules.js global.js syntax.js bool.js \
	unop.js binop.js misc.js conversion.js class.js evalexpr.js wabbit.js \
	number.js lex.js anyref.js string.js vector.js

# Tests that should fail
FAILOBJS=binop-mismatch.fail.js

.PHONY: test test-success test-failure clean snake

test: test-success test-failure snake

test-success: $(OBJS)
	@for i in $^; do echo $(JS) $(JSOPTS) $$i; $(JS) $(JSOPTS) $$i; done

test-failure: $(FAILOBJS)

clean:
	rm -f $(OBJS) $(FAILOBJS) $(FASLS) snake.metawasm.js binarize

snake: snake.js snake.wasm

snake.js snake.metawasm.js: snake.swat Makefile $(FASLS)
	$(SWAT) --js+wasm $<

$(OBJS) $(FAILOBJS): Makefile $(FASLS)

snake.wasm: binarize

%.js : %.swat
	$(SWAT) --js $<

%.fail.js : %.swat
	$(SWAT) --fail $<
	touch $@

%.fasl : %.sch
	$(COMPILE) $<

%.wasm : %.metawasm.js
	$(JS) $(JSOPTS) $< > TEMP
	./binarize TEMP $@
	@rm -f TEMP
