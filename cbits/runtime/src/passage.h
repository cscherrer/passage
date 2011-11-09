#ifndef __BAYESIAN_DSL_H_INCLUDED
#define __BAYESIAN_DSL_H_INCLUDED

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <omp.h>

extern unsigned long number_of_samples;
extern unsigned long steps_per_sample;
extern unsigned long warm_up_steps;
extern unsigned long num_threads;
extern int have_seed;

#ifdef __USE_MERSENNE
#include "mt19937ar.h"
#else
#include "kiss.h"
#endif

/* generates a random number on [0,1]-real-interval */
static inline
double genrand_real1(void) {
  return genrand_int32()*(1.0/4294967295.0);
  /* divided by 2^32-1 */
}

/* generates a random number on [0,1)-real-interval */
static inline
double genrand_real2() {
  return genrand_int32()*(1.0/4294967296.0);
  /* divided by 2^32 */
}

/* generates a random number on (0,1)-real-interval */
static inline
double genrand_real3() {
  return (((double)genrand_int32()) + 0.5)*(1.0/4294967296.0);
  /* divided by 2^32 */
}

/* Return a double-random value in [lo, hi] */
static inline
double getRandomRange(double lo, double hi) {
  return lo + (hi - lo) * genrand_real1();
}

/* Return a double-random value in [lo, hi) */
static inline
double getRandomRangeOpenRight(double lo, double hi) {
  return lo + (hi - lo) * genrand_real2();
}

static inline
double square (double x) { return x * x; }


static inline
void progress(unsigned long n) {
  int l = fprintf(stderr, "%lu", n);
  l += fprintf(stderr," of %lu (%2lu%%)", number_of_samples,
                                            100 * n / number_of_samples);
  for (; l > 0; --l) putc('\b', stderr);
}


void crash_out_of_bounds(int line) __attribute__((noreturn));

/* Generic samplers */


double slice_real(double (*ll)(double), double width, double x0);
double tune_slice_real(double (*ll)(double), double *width, double x0);
double slice_pos_real
  (double (*ll)(double), double width, double left, double x0);
double tune_slice_pos_real
  (double (*ll)(double), double *w, double left, double x0);
double slice_real_left_right
  ( double (*ll)(double)
  , double left
  , double right
  , double x0
  );

double slice_discrete_right(double (*ll)(double), double n, double x);
double slice_discrete(double (*ll)(double), double x0);

#define SLICE_NAME(var)         slice_##var
#define SLICE_TUNE_NAME(var)    slice_tune_##var
#define INIT_DET_VARS_NAME(var) init_det_vars_##var
#define LL_FUN_NAME(var)        ll_##var
#define WIDTH_NAME(var)         w_##var

#define SLICE(x)          SLICE_NAME(x)
#define SLICE_TUNE(x)     SLICE_TUNE_NAME(x)
#define INIT_DET_VARS(x)  INIT_DET_VARS_NAME(x)
#define LL_FUN(x)         LL_FUN_NAME(x)
#define WIDTH(x)          WIDTH_NAME(x)


#endif /* __BAYESIAN_DSL_H_INCLUDED */
