/* Metropolis-Hastings over the positive reals */
/* static inline */
double SLICE(VAR)(double x0)
{
  double y0;

  y0 = LL_FUN(VAR)(x0);

  // x1 is the proposal for the next value
  double x1    = -x0 * log(genrand_real3());
  double y1    = LL_FUN(VAR)(x1);

  double rat = x0 / x1;
  // logP is the log of the probability of transitioning to x1
  double logP = y1 - y0 + log(rat) + 1/rat - rat;

  if(logP > 0) {
    return x1;
  }
  else {
    if(log(genrand_real3()) < logP) {
      return x1;
    }
    else {
      return x0;
    }
  }

