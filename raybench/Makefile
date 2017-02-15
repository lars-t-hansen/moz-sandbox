CONFIG=-DPARTITIONING=true -DSHADOWS=true -DANTIALIAS=true -DREFLECTION=2 -DOUT_PPM_STDOUT

CXXFLAGS=-std=c++11 -O3

raybench: raybench.cpp Makefile
	/opt/local/bin/clang++-mp-3.9 $(CXXFLAGS) $(CONFIG) -o raybench raybench.cpp
