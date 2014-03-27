/*
 * Copyright (c) 2011, Intel Corporation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, 
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice, 
 *   this list of conditions and the following disclaimer in the documentation 
 *   and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF 
 * THE POSSIBILITY OF SUCH DAMAGE.
 *
 */


/* render() is the entry point to this script */

var numFrames = 20;
var maxIterations = 512;
var width, height;
var zoomFactor = 1.0;
var mandelPar;

// Sequential version
function computeSetSequential(zoomFactor, maxIterations) {
    var numPoints = width*height;
    var result = new Float64Array(numPoints);
    for(var x = 0; x < width; x++) {
        for(var y = 0; y < height; y++) {
            var Cr = ((x/width)-0.5)/zoomFactor*2.0-0.73;
            var Ci = ((y/height)-0.5)/zoomFactor*2.0-0.237;
            var I = 0, R = 0, I2 = 0, R2 = 0;
            var n = 0;
            while ((R2+I2 < 2.0) && (n < maxIterations)) {
                I = (R+R)*I+Ci;
                R = R2-I2+Cr;
                R2 = R*R;
                I2 = I*I;
                n++;
            } 
            result[y*width+x] = n;
        }
    }
    return result;
}

// Parallel version
// el is 0, as we are doing a mapPar on a freshly created ArrayType instance
function computeSetParallel(el, iv) {
    var y = (iv/height) << 0;
    var x = (iv - y*height) << 0;
    var Cr = (((x/width)-0.5)/zoomFactor)*2.0-0.73;
    var Ci = (((y/height)-0.5)/zoomFactor)*2.0-0.237;
    var I = 0, R = 0, I2 = 0, R2 = 0;
    var n = 0;
    while ((R2+I2 < 2.0) && (n < maxIterations)) {
       I = (R+R)*I+Ci;
       R = R2-I2+Cr;
       R2 = R*R;
       I2 = I*I;
       n++;
    } 
    return n;
}

function computeFrame(isParallel) {
    var mandelbrot;
    if(isParallel) 
        mandelbrot = mandelPar.mapPar(computeSetParallel);
    else
        mandelbrot = computeSetSequential(zoomFactor, maxIterations);
    zoomFactor *= 1.01;
}

var T = TypedObject;

/* This is the entry point for the script */

function render () {
    var modeStr;
    width = 512; height = 512;
    var type = T.float64.array(width*height);
    mandelPar = new type();

    /* Do sequential runs */

    var modeStr = "sequential";
    // Warmup ?
    computeFrame(false);
    start_time = Date.now();
    for(var i = 0; i < numFrames; i++) {
        computeFrame(false);
    }
    elapsed = Date.now() - start_time;

    print("[" + modeStr + "] : Took " + elapsed + " ms for " + numFrames + " iterations - " + (numFrames*1000/elapsed).toFixed(2) + " fps");



    /* Do parallel runs */

    var modeStr = "parallel";
    // Warmup ?
    computeFrame(true);
    var start_time = Date.now();
    for(var i = 0; i < numFrames; i++) {
        computeFrame(true);
    }
    var elapsed = Date.now() - start_time;
    print("[" + modeStr + "] : Took " + elapsed + " ms for " + numFrames + " iterations - " + (numFrames*1000/elapsed).toFixed(2) + " fps");
    

}
render();
