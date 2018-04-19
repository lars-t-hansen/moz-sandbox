JS=~/m-i/js/src/build-release/dist/bin/js
SWAT=~/moz/sandbox/swat/swat

OBJS=fib.wast.js let.wast.js while.wast.js loop.wast.js multi-modules.wast.js

test: $(OBJS)
	-for i in $(OBJS); do echo $(JS) $$i; $(JS) $$i; done

clean:
	rm -f $(OBJS)

%.wast.js : %.swat swat.sch
	$(SWAT) --js $<
