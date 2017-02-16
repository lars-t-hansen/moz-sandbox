# Output options:
#
#   HEADLESS    = just print timing info on stderr
#   PPM_STDOUT  = dump a ppm on stdout (plus timing info on stderr)
#   SDL_BROWSER = render to a canvas using SDL (plus timing info on stderr)

CONFIG=-DPARTITIONING=true -DSHADOWS=true -DANTIALIAS=true -DREFLECTION=2
CXXFLAGS=-std=c++11 -O3

# Mac/Linux console
# CXX=/opt/local/bin/clang++-mp-3.9
# EXEFILE=raybench
# OUTPUT=-DPPM_STDOUT

# JS targets using emscripten.
#
# For WebAssembly add '-s WASM=1' to CXX.  In this case you want the
# 'incoming' branch of the emsdk, see the README.md file in the emsdk
# for information.
#
# To compile for asm.js, just remove the '-s WASM=1'

# Emscripten, for the browser.
# CXX=emcc -s WASM=1
# EXEFILE=raybench.html
# OUTPUT=-DSDL_BROWSER

# Emscripten, for the shell.
CXX=emcc -s WASM=1
EXEFILE=raybench.js
OUTPUT=-DHEADLESS

raybench: raybench.cpp Makefile
	$(CXX) $(CXXFLAGS) $(CONFIG) $(OUTPUT) -o $(EXEFILE) raybench.cpp

