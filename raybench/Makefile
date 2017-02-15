# Output options:
#
#   HEADLESS    = just stderr timing info
#   PPM_STDOUT  = dump a ppm on stdout
#   SDL_BROWSER = render to a canvas using SDL

CONFIG=-DPARTITIONING=true -DSHADOWS=true -DANTIALIAS=true -DREFLECTION=2
CXXFLAGS=-std=c++11 -O3

# Mac/Linux console
# CXX=/opt/local/bin/clang++-mp-3.9
# EXEFILE=raybench
# OUTPUT=-DPPM_STDOUT

# Emscripten, for the browser
# CXX=emcc
# EXEFILE=raybench.html
# OUTPUT=-DSDL_BROWSER

# Emscripten, for the shell
CXX=emcc
EXEFILE=raybench.js
OUTPUT=-DHEADLESS

raybench: raybench.cpp Makefile
	$(CXX) $(CXXFLAGS) $(CONFIG) $(OUTPUT) -o $(EXEFILE) raybench.cpp
