/* -*- mode: javascript -*- */

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Author: Lars T Hansen, lth@acm.org / lhansen@mozilla.com
 */

/*
 * Ray tracer, largely out of Shirley & Marschner 3rd Ed.
 * Traces a scene and writes to a canvas.
 */

// What takes time here is antialiasing.  To allow this program to scale as a
// benchmark, we could use a smaller antialiasing grid (2x2 or 3x3, not 4x4).
//
// Also, partitioning speeds the program by a factor of 3.5 or so, so disabling
// that would make it more challenging still.

#include <vector>
#include <cmath>
#include <cstdlib>
#include <cstdio>
#include <cstdarg>
#include <sys/time.h>

#ifdef __EMSCRIPTEN__
#  include <emscripten.h>
#endif
#ifdef SDL_BROWSER
#  include <SDL/SDL.h>
#endif

using std::vector;

// CONFIGURATION

// Floating point type
#if !defined(DOUBLE) && !defined(FLOAT)
#  define DOUBLE
#endif

#ifdef DOUBLE
typedef double Float;
#  define Sqrt sqrt
#  define Pow pow
#  define Sin sin
#  define Cos cos
#else
#  error "Defines for FLOAT"
#endif

// Rendering
#if !defined(PPM_STDOUT) && !defined(SDL_BROWSER) && !defined(HEADLESS)
#  define PPM_STDOUT
#endif

// Partition the scene for faster tracing
#if !defined(PARTITIONING)
#  define PARTITIONING true
#endif

// Scene & tracing parameters
#ifndef HEIGHT
#  define HEIGHT 600
#endif

#ifndef WIDTH
#  define WIDTH 800;
#endif

#ifndef SHADOWS
#  define SHADOWS true
#endif

#ifndef REFLECTION
#  define REFLECTION 2
#endif

#ifndef ANTIALIAS
#  define ANTIALIAS true
#endif

const bool g_partitioning = PARTITIONING;

const uint32_t g_height = HEIGHT;
const uint32_t g_width = WIDTH;

const bool g_shadows = SHADOWS;	                // Compute object shadows

const bool g_reflection = (REFLECTION != 0);    // Compute object reflections
const uint32_t g_reflection_depth = REFLECTION; //   to this depth

const bool g_antialias = ANTIALIAS;             // Antialias the image (expensive but very pretty)

// Viewport
const Float g_left = -2;
const Float g_right = 2;
const Float g_top = 1.5;
const Float g_bottom = -1.5;

// END CONFIGURATION

void CRASH(const char* msg) {
    fprintf(stderr, "CRASH: %s\n", msg);
    exit(1);
}

void WARNING(const char* msg) {
    fprintf(stderr, "WARNING: %s\n", msg);
}

void PRINT(const char* s)
{
    fprintf(stderr, "%s", s);
}

uint64_t timestamp() {
    struct timeval tp;
    gettimeofday(&tp, nullptr);
    return uint64_t(tp.tv_sec)*1000000 + tp.tv_usec;
}

struct Vec3 {
    Float x;
    Float y;
    Float z;

    Vec3() : x(0), y(0), z(0) {}

    Vec3(Float x, Float y, Float z)
	: x(x)
	, y(y)
	, z(z)
    {}
};

// Vector parameter type, this will change with SIMD
#define V3P const Vec3&

const Float SENTINEL = 1e32;
const Float EPS = 0.00001;

// TODO: Clearly we can use operator overloading here.

static inline Vec3 add(V3P a, V3P b) {
    return Vec3(a.x+b.x, a.y+b.y, a.z+b.z);
}

static inline Vec3 addi(V3P a, Float c) {
    return Vec3(a.x+c, a.y+c, a.z+c);
}

static inline Vec3 sub(V3P a, V3P b) {
    return Vec3(a.x-b.x, a.y-b.y, a.z-b.z);
}

static inline Vec3 subi(V3P a, Float c) {
    return Vec3(a.x-c, a.y-c, a.z-c);
}

static inline Vec3 muli(V3P a, Float c) {
    return Vec3(a.x*c, a.y*c, a.z*c);
}

static inline Vec3 divi(V3P a, Float c) {
    return Vec3(a.x/c, a.y/c, a.z/c);
}

static inline Vec3 inv(V3P a) {
    return Vec3(1/a.x, 1/a.y, 1/a.z);
}

static inline Vec3 neg(V3P a) {
    return Vec3(-a.x, -a.y, -a.z);
}

static inline Float length(V3P a) {
    return Sqrt(a.x*a.x + a.y*a.y + a.z*a.z);
}

static inline Vec3 normalize(V3P a) {
    Float d = length(a);
    return Vec3(a.x/d, a.y/d, a.z/d);
}

static inline Vec3 cross(V3P a, V3P b) {
    return Vec3(a.y*b.z - a.z*b.y, a.z*b.x - a.x*b.z, a.x*b.y - a.y*b.x);
}

static inline Float dot(V3P a, V3P b) {
    return a.x*b.x + a.y*b.y + a.z*b.z;
}

static inline Float Max(Float a, Float b) {
    return a > b ? a : b;
}

static inline Float Max(Float a, Float b, Float c) {
    Float t = a > b ? a : b;
    return c > t ? c : t;
}

static inline Float Min(Float a, Float b) {
    return a < b ? a : b;
}

static inline Float Min(Float a, Float b, Float c) {
    Float t = a < b ? a : b;
    return c < t ? c : t;
}

struct Material {
    Vec3 diffuse;
    Vec3 specular;
    Float shininess;
    Vec3 ambient;
    Float mirror;

    Material(V3P diffuse, V3P specular, Float shininess, V3P ambient, Float mirror)
	: diffuse(diffuse)
	, specular(specular)
	, shininess(shininess)
	, ambient(ambient)
	, mirror(mirror)
    {}

    Material()
	: diffuse(Vec3(0,0,0))
	, specular(Vec3(0,0,0))
	, shininess(0)
	, ambient(Vec3(0,0,0))
	, mirror(0)
    {}
};

struct Bounds {
    Float xmin;
    Float xmax;
    Float ymin;
    Float ymax;
    Float zmin;
    Float zmax;

    Bounds(Float xmin, Float xmax, Float ymin, Float ymax, Float zmin, Float zmax)
	: xmin(xmin)
	, xmax(xmax)
	, ymin(ymin)
	, ymax(ymax)
	, zmin(zmin)
	, zmax(zmax)
    {}
};

class Surface
{
public:
    Material material;

    Surface(const Material& material)
	: material(material)
    {}

    virtual Surface* intersect(V3P eye, V3P ray, Float min, Float max, Float* distance) = 0;
    virtual Vec3 normal(V3P p) = 0;
    virtual Bounds bounds() = 0;
    virtual Vec3 center() = 0;
    virtual void debug(void (*print)(const char* s), uint32_t level) = 0;
};

class Volume : public Surface
{
    Bounds   bounds_;
    Surface* left_;
    Surface* right_;

public:
    Volume(Bounds bounds, Surface* left, Surface* right)
	: Surface(Material())
	, bounds_(bounds)
	, left_(left)
	, right_(right)
    {}

    Surface* intersect(V3P eye, V3P ray, Float min, Float max, Float* distance) {
	// Test volume intersection.
	Float tmin = 0, tmax = 0, tymin = 0, tymax = 0, tzmin = 0, tzmax = 0;
	{
	    Float a = 1/ray.x;
	    if (a >= 0) {
		tmin = (bounds_.xmin - eye.x)*a;
		tmax = (bounds_.xmax - eye.x)*a;
	    }
	    else {
		tmin = (bounds_.xmax - eye.x)*a;
		tmax = (bounds_.xmin - eye.x)*a;
	    }
	}
	{
	    Float a = 1/ray.y;
	    if (a >= 0) {
		tymin = (bounds_.ymin - eye.y)*a;
		tymax = (bounds_.ymax - eye.y)*a;
	    }
	    else {
		tymin = (bounds_.ymax - eye.y)*a;
		tymax = (bounds_.ymin - eye.y)*a;
	    }
	}
	if (tmin > tymax || tymin > tmax)
	    return nullptr;
	if (tymin > tmin)
	    tmin = tymin;
	if (tymax < tmax)
	    tmax = tymax;
	{
	    Float a = 1/ray.z;
	    if (a >= 0) {
		tzmin = (bounds_.zmin - eye.z)*a;
		tzmax = (bounds_.zmax - eye.z)*a;
	    }
	    else {
		tzmin = (bounds_.zmax - eye.z)*a;
		tzmax = (bounds_.zmin - eye.z)*a;
	    }
	}
	if (tmin > tzmax || tzmin > tmax)
	    return nullptr;
	if (tzmin > tmin)
	    tmin = tzmin;
	if (tzmax < tmax)
	    tmax = tzmax;

	if (!(tmin < max && tmax > min))
	    return nullptr;

	// Then test object intersection.
	Float d1 = 0;
	Surface* r1 = left_->intersect(eye, ray, min, max, &d1);
	if (right_) {
	    Float d2 = 0;
	    Surface* r2 = right_->intersect(eye, ray, min, max, &d2);
	    if (r2 && (!r1 || d2 < d1)) {
		*distance = d2;
		return r2;
	    }
	}
	*distance = d1;
	return r1;
    }

    Bounds bounds() {
	return bounds_;
    }

    Vec3 normal(V3P p) {
	CRASH("Normal not implemented for Volume");
	return Vec3(0,0,0);
    }

    Vec3 center() {
	CRASH("Center not implemented for Volume");
	return Vec3(0,0,0);
    }

    void debug(void (*print)(const char* s), uint32_t level) {
	print("[");
	left_->debug(print, level+1);
	if (right_) {
	    print(",\n");
	    for ( uint32_t i=0 ; i < level ; i++ )
		print(" ");
	    right_->debug(print, level+1);
	}
	print("]");
    }
};

class Jumble : public Surface
{
    vector<Surface*> surfaces;

public:
    Jumble(vector<Surface*>& world)
	: Surface(Material())
    {
	surfaces.assign(world.begin(), world.end());
    }

    Surface* intersect(V3P eye, V3P ray, Float min, Float max, Float* distance) {
	Surface* min_obj = nullptr;
	Float min_dist = 1e100;

	for ( Surface* surface : surfaces ) {
	    Float dist = 0;
	    Surface* obj = surface->intersect(eye, ray, min, max, &dist);
	    if (obj) {
		if (dist < min_dist) {
		    min_obj = obj;
		    min_dist = dist;
		}
	    }
	}
	*distance = min_dist;
	return min_obj;
    }

    Vec3 normal(V3P p) {
	CRASH("Normal not implemented for Jumble");
	return Vec3(0,0,0);
    }

    Bounds bounds() {
	CRASH("Bounds not implemented for Jumble");
	return Bounds(0,0,0,0,0,0);
    }

    Vec3 center() {
	CRASH("Center not implemented for Jumble");
	return Vec3(0,0,0);
    }

    void debug(void (*print)(const char* s), uint32_t level) {
    }
};

class Sphere : public Surface
{
    Vec3 center_;
    Float radius_;

public:
    Sphere(const Material& material, V3P center, Float radius)
	: Surface(material)
	, center_(center)
	, radius_(radius)
    {}

    Surface* intersect(V3P eye, V3P ray, Float min, Float max, Float* distance) {
	Float DdotD = dot(ray, ray);
	Vec3 EminusC = sub(eye, center_);
	Float B = dot(ray, EminusC);
	Float disc = B*B - DdotD*(dot(EminusC,EminusC) - radius_*radius_);
	if (disc < 0.0)
	    return nullptr;
	Float s1 = (-B + Sqrt(disc))/DdotD;
	Float s2 = (-B - Sqrt(disc))/DdotD;
	// Here return the smallest of s1 and s2 after filtering for _min and _max
	if (s1 < min || s1 > max)
	    s1 = SENTINEL;
	if (s2 < min || s2 > max)
	    s2 = SENTINEL;
	Float dist = Min(s1, s2);
	if (dist == SENTINEL)
	    return nullptr;
	*distance = dist;
	return this;
    }

    Vec3 normal(V3P p) {
	return divi(sub(p, center_), radius_);
    }

    Bounds bounds() {
	return Bounds(center_.x - radius_, center_.x + radius_,
		      center_.y - radius_, center_.y + radius_,
		      center_.z - radius_, center_.z + radius_);
    }

    Vec3 center() {
	return center_;
    }

    void debug(void (*print)(const char* s), uint32_t level) {
	char buf[256];
	sprintf(buf, "(S c=(%g,%g,%g) r=%g)", center_.x, center_.y, center_.z, radius_);
	print(buf);
    }
};

class Triangle : public Surface
{
    Vec3 v1;
    Vec3 v2;
    Vec3 v3;
    Vec3 norm;

public:
    Triangle(const Material& material, V3P v1, V3P v2, V3P v3)
	: Surface(material)
	, v1(v1)
	, v2(v2)
	, v3(v3)
	, norm(normalize(cross(sub(v2, v1), sub(v3, v1))))
    {}

    Surface* intersect(V3P eye, V3P ray, Float min, Float max, Float* distance) {
	// TODO: observe that values that do not depend on g, h, and i can be precomputed
	// and stored with the triangle (for a given eye position), at some (possibly significant)
	// space cost.  Notably the numerator of "t" is invariant, as are many factors of the
	// numerator of "gamma".
	Float a = v1.x - v2.x;
	Float b = v1.y - v2.y;
	Float c = v1.z - v2.z;
	Float d = v1.x - v3.x;
	Float e = v1.y - v3.y;
	Float f = v1.z - v3.z;
	Float g = ray.x;
	Float h = ray.y;
	Float i = ray.z;
	Float j = v1.x - eye.x;
	Float k = v1.y - eye.y;
	Float l = v1.z - eye.z;
	Float M = a*(e*i - h*f) + b*(g*f - d*i) + c*(d*h - e*g);
	Float t = -((f*(a*k - j*b) + e*(j*c - a*l) + d*(b*l - k*c))/M);
	if (t < min || t > max)
	    return nullptr;
	Float gamma = (i*(a*k - j*b) + h*(j*c - a*l) + g*(b*l - k*c))/M;
	if (gamma < 0 || gamma > 1.0)
	    return nullptr;
	Float beta = (j*(e*i - h*f) + k*(g*f - d*i) + l*(d*h - e*g))/M;
	if (beta < 0.0 || beta > 1.0 - gamma)
	    return nullptr;
	*distance = t;
	return this;
    }

    Vec3 normal(V3P p) {
	return norm;
    }

    Bounds bounds() {
	return Bounds(Min(v1.x, v2.x, v3.x), Max(v1.x, v2.x, v3.x),
		      Min(v1.y, v2.y, v3.y), Max(v1.y, v2.y, v3.y),
		      Min(v1.z, v2.z, v3.z), Max(v1.z, v2.z, v3.z));
    }

    Vec3 center() {
	return Vec3((v1.x + v2.x + v3.x)/3,
		    (v1.y + v2.y + v3.y)/3,
		    (v1.z + v2.z + v3.z)/3);
    }

    void debug(void (*print)(const char* s), uint32_t level) {
	char buf[1024];
	sprintf(buf, "[T (%g,%g,%g) (%g,%g,%g) (%g,%g,%g)]", v1.x, v1.y, v1.z, v2.x, v2.y, v2.z, v3.x, v3.y, v3.z);
	print(buf);
    }
};

// "Color" is a Vec3 representing RGB scaled by 256.
// "RGBA" is a uint32_t representing r,g,b,a in the range 0..255

Vec3 colorFromRGB(uint32_t r, uint32_t g, uint32_t b) {
    return Vec3(r/256.0, g/256.0, b/256.0);
}

uint32_t rgbaFromColor(V3P color)
{
    return (255<<24) | (uint32_t(255*color.z)<<16) | (uint32_t(255*color.y)<<8) | uint32_t(255*color.x);
}

void componentsFromRgba(uint32_t rgba, uint8_t* r, uint8_t* g, uint8_t* b, uint8_t* a)
{
    *r = rgba;
    *g = (rgba >> 8);
    *b = (rgba >> 16);
    *a = (rgba >> 24);
}

class Bitmap
{
    uint32_t height;
    uint32_t width;
    uint32_t* const data;

public:
    Bitmap(uint32_t height, uint32_t width, V3P color)
	: height(height)
	, width(width)
	, data(new uint32_t[height*width])
    {
	if (!data)
	    CRASH("Failed to allocate array");

	uint32_t c = rgbaFromColor(color);
	for ( uint32_t i=0, l=width*height ; i < l ; i++ )
	    data[i] = c;
    }

    // For debugging only
    uint32_t ref(uint32_t y, uint32_t x) {
	return data[(height-y)*width + x];
    }

    // Not a hot function
    void setColor(uint32_t y, uint32_t x, V3P v) {
	data[(height-y)*width + x] = rgbaFromColor(v);
    }
};


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

Surface* setStage(Vec3* eye, Vec3* light, Vec3* background);
void trace(uint32_t ymin, uint32_t ylim, uint32_t xmin, uint32_t xlim, V3P eye, V3P light, V3P background, Surface* world, Bitmap* bits);

int main(int argc, char** argv)
{
    Vec3 eye;
    Vec3 light;
    Vec3 background;
    Surface* world;

    {
	uint64_t then = timestamp();
	world = setStage(&eye, &light, &background);
	uint64_t now = timestamp();
	fprintf(stderr, "Setup time: %g ms\n", (now-then) / 1000.0);
    }

    Bitmap bits(g_height, g_width, colorFromRGB(152, 251, 152));

    {
	uint64_t then = timestamp();
	trace(0, g_height, 0, g_width, eye, light, background, world, &bits);
	uint64_t now = timestamp();
	fprintf(stderr, "Render time: %g ms\n", (now-then) / 1000.0);
    }

#ifdef PPM_STDOUT

#  ifdef __EMSCRIPTEN__
#    error "PPM generation does not work with emscripten-compiled code"
#  endif

    printf("P6 %d %d 255\n", g_width, g_height);
    for ( uint32_t h=g_height ; h > 0 ; h-- ) {
	for ( uint32_t w=0 ; w < g_width ; w++ ) {
	    uint8_t r, g, b, a;
	    componentsFromRgba(bits.ref(h-1,w), &r, &g, &b, &a);
	    putchar(r);
	    putchar(g);
	    putchar(b);
	}
    }
#endif

#ifdef SDL_BROWSER
    SDL_Init(SDL_INIT_VIDEO);
    SDL_Surface *screen = SDL_SetVideoMode(g_width, g_height, 32, SDL_SWSURFACE);

    if (SDL_MUSTLOCK(screen))
	SDL_LockSurface(screen);

    for (uint32_t y = 0; y < g_height ; y++ ) {
	for (uint32_t x = 0; x < g_width; x++) {
	    uint8_t r, g, b, a;
	    componentsFromRgba(bits.ref(y-1, x), &r, &g, &b, &a);
	    *((Uint32*)screen->pixels + (g_height-y-1) * g_width + x) = SDL_MapRGBA(screen->format, r, g, b, a);
	}
    }

    if (SDL_MUSTLOCK(screen))
	SDL_UnlockSurface(screen);
#endif
}


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

Vec3 g_eye;
Vec3 g_background;
Vec3 g_light;
Surface* g_world;
Bitmap* g_bits;

void traceWithoutAntialias(uint32_t ymin, uint32_t ylim, uint32_t xmin, uint32_t xlim);
void traceWithAntialias(uint32_t ymin, uint32_t ylim, uint32_t xmin, uint32_t xlim);
Vec3 raycolor(V3P eye, V3P ray, Float t0, Float t1, uint32_t depth);

void trace(uint32_t ymin, uint32_t ylim, uint32_t xmin, uint32_t xlim, V3P eye, V3P light, V3P background, Surface* world, Bitmap* bits)
{
    // Easiest to keep these in globals.
    g_eye = eye;
    g_light = light;
    g_background = background;
    g_world = world;
    g_bits = bits;
    if (g_antialias)
	traceWithAntialias(ymin, ylim, xmin, xlim);
    else
	traceWithoutAntialias(ymin, ylim, xmin, xlim);
}

void traceWithoutAntialias(uint32_t ymin, uint32_t ylim, uint32_t xmin, uint32_t xlim)
{
    for ( uint32_t h=ymin ; h < ylim ; h++ ) {
	for ( uint32_t w=xmin ; w < xlim ; w++ ) {
	    Float u = g_left + (g_right - g_left)*(w + 0.5)/g_width;
	    Float v = g_bottom + (g_top - g_bottom)*(h + 0.5)/g_height;
	    Vec3 ray = Vec3(u, v, -g_eye.z);
	    Vec3 col = raycolor(g_eye, ray, 0, SENTINEL, g_reflection_depth);
	    g_bits->setColor(h, w, col);
	}
    }
}

const Float random_numbers[] = {
    0.495,0.840,0.636,0.407,0.026,0.547,0.223,0.349,0.033,0.643,0.558,0.481,0.039,
    0.175,0.169,0.606,0.638,0.364,0.709,0.814,0.206,0.346,0.812,0.603,0.969,0.888,
    0.294,0.824,0.410,0.467,0.029,0.706,0.314
};

void traceWithAntialias(uint32_t ymin, uint32_t ylim, uint32_t xmin, uint32_t xlim)
{
    uint32_t k = 0;
    for ( uint32_t h=ymin ; h < ylim ; h++ ) {
	for ( uint32_t w=xmin ; w < xlim ; w++ ) {
	    // Simple stratified sampling, cf Shirley&Marschner ch 13 and a fast "random" function.
	    const uint32_t n = 4;
	    //var k = h % 32;
	    uint32_t rand = k % 2;
	    Vec3 c;
	    k++;
	    for ( uint32_t p=0 ; p < n ; p++ ) {
		for ( uint32_t q=0 ; q < n ; q++ ) {
		    Float jx = random_numbers[rand]; rand=rand+1;
		    Float jy = random_numbers[rand]; rand=rand+1;
		    Float u = g_left + (g_right - g_left)*(w + (p + jx)/n)/g_width;
		    Float v = g_bottom + (g_top - g_bottom)*(h + (q + jy)/n)/g_height;
		    Vec3 ray = Vec3(u, v, -g_eye.z);
		    c = add(c, raycolor(g_eye, ray, 0.0, SENTINEL, g_reflection_depth));
		}
	    }
	    g_bits->setColor(h, w, divi(c, n*n));
	}
    }
}

// Clamping c is not necessary provided the three color components by
// themselves never add up to more than 1, and shininess == 0 or shininess >= 1.
//
// TODO: lighting intensity is baked into the material here, but we probably want
// to factor that out and somehow attenuate light with distance from the light source,
// for diffuse and specular lighting.

Vec3 raycolor(V3P eye, V3P ray, Float t0, Float t1, uint32_t depth)
{
    Float dist;
    Surface* obj = g_world->intersect(eye, ray, t0, t1, &dist);

    if (obj) {
	Material& m = obj->material;
	Vec3 p = add(eye, muli(ray, dist));
	Vec3 n1 = obj->normal(p);
	Vec3 l1 = normalize(sub(g_light, p));
	Vec3 c = m.ambient;
	Surface* min_obj = nullptr;

	if (g_shadows) {
	    Float tmp;
	    min_obj = g_world->intersect(add(p, muli(l1, EPS)), l1, EPS, SENTINEL, &tmp);
	}
	if (!min_obj) {
	    const Float diffuse = Max(0.0, dot(n1,l1));
	    const Vec3 v1 = normalize(neg(ray));
	    const Vec3 h1 = normalize(add(v1, l1));
	    const Float specular = Pow(Max(0.0, dot(n1, h1)), m.shininess);
	    c = add(c, add(muli(m.diffuse, diffuse), muli(m.specular, specular)));
	    if (g_reflection) {
		if (depth > 0 && m.mirror != 0.0) {
		    const Vec3 r = sub(ray, muli(n1, 2.0*dot(ray, n1)));
		    c = add(c, muli(raycolor(add(p, muli(r, EPS)), r, EPS, SENTINEL, depth-1), m.mirror));
		}
	    }
	}
	return c;
    }
    return g_background;
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// Colors: http://kb.iu.edu/data/aetf.html

const Vec3 paleGreen = colorFromRGB(152, 251, 152);
const Vec3 darkGray = colorFromRGB(169, 169, 169);
const Vec3 yellow = colorFromRGB(256, 256, 0);
const Vec3 red = colorFromRGB(256, 0, 0);
const Vec3 blue = colorFromRGB(0, 0, 256);

// Not restricted to a rectangle, actually
void rectangle(vector<Surface*>& world, const Material& m, V3P v1, V3P v2, V3P v3, V3P v4)
{
    world.push_back(new Triangle(m, v1, v2, v3));
    world.push_back(new Triangle(m, v1, v3, v4));
}

// Vertices are for front and back faces, both counterclockwise as seen
// from the outside.
// Not restricted to a cube, actually.
void cube(vector<Surface*>& world, const Material& m, V3P v1, V3P v2, V3P v3, V3P v4, V3P v5, V3P v6, V3P v7, V3P v8)
{
    rectangle(world, m, v1, v2, v3, v4);  // front
    rectangle(world, m, v2, v5, v8, v3);  // right
    rectangle(world, m, v6, v1, v4, v7);  // left
    rectangle(world, m, v5, v6, v7, v8);  // back
    rectangle(world, m, v4, v3, v8, v7);  // top
    rectangle(world, m, v6, v1, v2, v5);  // bottom
}

const Float MAXBOUND = 1e100;
const Float MINBOUND = -MAXBOUND;

Bounds computeBounds(vector<Surface*>& surfaces)
{
    Float xmin = MAXBOUND, xmax = MINBOUND, ymin = MAXBOUND, ymax = MINBOUND,
	  zmin = MAXBOUND, zmax = MINBOUND;
    for ( Surface *s : surfaces ) {
	Bounds b = s->bounds();
	xmin = Min(xmin, b.xmin);
	xmax = Max(xmax, b.xmax);
	ymin = Min(ymin, b.ymin);
	ymax = Max(ymax, b.ymax);
	zmin = Min(zmin, b.zmin);
	zmax = Max(zmax, b.zmax);
    }
    return Bounds(xmin, xmax, ymin, ymax, zmin, zmax);
}

// This is not quite right, cf the Jumble object.  The bug could be here, or in
// the intersection algorithm.

Surface* partition(vector<Surface*>& surfaces, Bounds bounds, uint32_t axis)
{
    Surface* left=nullptr;
    Surface* right=nullptr;
    if (surfaces.size() == 1) {
	left = surfaces[0];
	right = nullptr;
    }
    else if (surfaces.size() == 2) {
	left = surfaces[0];
	right = surfaces[1];
    }
    else {
	// We really should choose the "best" partitioning here, ie the most
	// even.  Instead we pick the first that works, and if we can't
	// partition among any axis we put the objects in a bag together and
	// trace them all.  There are other strategies.
	uint32_t safety = 4;
	vector<Surface*> lobj;
	vector<Surface*> robj;
	for (;;) {
	    if (!--safety) {
		WARNING("Degenerate parition");
		return new Jumble(surfaces);
	    }
	    Float mid = 0;
	    Float center = 0;
	    for ( Surface* s : surfaces) {
		switch (axis) {
		  case 0:
		    mid = (bounds.xmax + bounds.xmin) / 2;
		    center = s->center().x;
		    break;
		  case 1:
		    mid = (bounds.ymax + bounds.ymin) / 2;
		    center = s->center().y;
		    break;
		  case 2:
		    mid = (bounds.zmax + bounds.zmin) / 2;
		    center = s->center().z;
		    break;
		}
		if (center <= mid)
		    lobj.push_back(s);
		else
		    robj.push_back(s);
	    }
	    axis = (axis + 1) % 3;
	    if (robj.size() && lobj.size())
		break;
	    lobj.clear();
	    robj.clear();
	}
	left = lobj.size() == 1 ? lobj[0] : partition(lobj, computeBounds(lobj), axis);
	right = robj.size() == 1 ? robj[0] : partition(robj, computeBounds(robj), axis);
    }
    return new Volume(bounds, left, right);
}

Surface* setStage(Vec3* eye, Vec3* light, Vec3* background)
{
    Material m1(Vec3(0.1, 0.2, 0.2), Vec3(0.3, 0.6, 0.6), 10, Vec3(0.05, 0.1, 0.1),  0);
    Material m2(Vec3(0.3, 0.3, 0.2), Vec3(0.6, 0.6, 0.4), 10, Vec3(0.1,  0.1, 0.05), 0);
    Material m3(Vec3(0.1, 0.0, 0.0), Vec3(0.8, 0.0, 0.0), 10, Vec3(0.1,  0.0, 0.0),  0);
    Material m4(muli(darkGray,0.4),  muli(darkGray,0.3), 100, muli(darkGray,0.3),    0.5);
    Material m5(muli(paleGreen,0.4), muli(paleGreen,0.4), 10, muli(paleGreen,0.2),   1.0);
    Material m6(muli(yellow,0.6),    Vec3(0, 0, 0),        0, muli(yellow,0.4),      0);
    Material m7(muli(red,0.6),       Vec3(0, 0, 0),        0, muli(red,0.4),         0);
    Material m8(muli(blue,0.6),      Vec3(0, 0, 0),        0, muli(blue,0.4),        0);

    vector<Surface*> world;

    world.push_back(new Sphere(m1, Vec3(-1, 1, -9), 1));
    world.push_back(new Sphere(m2, Vec3(1.5, 1, 0), 0.75));
    world.push_back(new Triangle(m1, Vec3(-1,0,0.75), Vec3(-0.75,0,0), Vec3(-0.75,1.5,0)));
    world.push_back(new Triangle(m3, Vec3(-2,0,0), Vec3(-0.5,0,0), Vec3(-0.5,2,0)));
    rectangle(world, m4, Vec3(-5,0,5), Vec3(5,0,5), Vec3(5,0,-40), Vec3(-5,0,-40));
    cube(world, m5, Vec3(1, 1.5, 1.5), Vec3(1.5, 1.5, 1.25), Vec3(1.5, 1.75, 1.25), Vec3(1, 1.75, 1.5),
	 Vec3(1.5, 1.5, 0.5), Vec3(1, 1.5, 0.75), Vec3(1, 1.75, 0.75), Vec3(1.5, 1.75, 0.5));
    for ( uint32_t i=0 ; i < 30 ; i++ )
	world.push_back(new Sphere(m6, Vec3((-0.6+(i*0.2)), (0.075+(i*0.05)), (1.5-(i*Cos(i/30.0)*0.5))), 0.075));
    for ( uint32_t i=0 ; i < 60 ; i++ )
	world.push_back(new Sphere(m7, Vec3((1+0.3*Sin(i*(3.14/16))), (0.075+(i*0.025)), (1+0.3*Cos(i*(3.14/16)))), 0.025));
    for ( uint32_t i=0 ; i < 60 ; i++ )
	world.push_back(new Sphere(m8, Vec3((1+0.3*Sin(i*(3.14/16))), (0.075+((i+8)*0.025)), (1+0.3*Cos(i*(3.14/16)))), 0.025));

    *eye        = Vec3(0.5, 0.75, 5);
    *light      = Vec3(g_left-1, g_top, 2);
    *background = colorFromRGB(25, 25, 112);

    if (g_partitioning)
	return partition(world, computeBounds(world), 0);

    return new Jumble(world);
}
