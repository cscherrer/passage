/* Independent Metropolis sampling over [0..n-1] */
/* static inline */
double SLICE(VAR)(double x) {
  double y;
  double n = RIGHT;
  y = floor(getRandomRangeOpenRight(0,n+1));
  return (log(genrand_real3()) < LL_FUN(VAR)(y) - LL_FUN(VAR)(x)) ? y : x;
}


