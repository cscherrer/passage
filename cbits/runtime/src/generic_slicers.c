#include "passage.h"

/* This duplicates code from the templates.
   It'd be nice to eliminate this, at a later stage.
*/

double slice_real(double (*ll)(double), double width, double x0)
{
  double x1;
  double r;
  double y;

  double lo;

  double left;
  double right;

  r = genrand_real3();
  y = log(r) + ll(x0);

  lo= getRandomRange(0, width);

  left = x0 - lo;
  while(ll(left) > y) left -= width;

  right = x0 - lo + width;
  while(ll(right) > y) right += width;


  for ( x1 = getRandomRange(left, right)
      ; ll(x1) < y
      ; x1 = getRandomRange(left, right)
      ) {
    if (x1 < x0) left = x1; else right = x1;
  }

  return x1;
}


double tune_slice_real(double (*ll)(double), double *width, double x0)
{
  double x1;
  double r;
  double y;

  double lo;

  double left;
  double right;

  int steps_out = 0;
  int steps_in = 0;

  r = genrand_real3();
  y = log(r) + ll(x0);

  lo= getRandomRange(0, *width);

  left = x0 - lo;
  while(ll(left) > y) { left -= *width; ++steps_out; }

  right = x0 - lo + *width;
  while(ll(right) > y) { right += *width; ++steps_out; }


  for ( x1 = getRandomRange(left, right)
      ; ll(x1) < y
      ; x1 = getRandomRange(left, right)
      ) {
    if (x1 < x0) left = x1; else right = x1;
    ++steps_in;
  }

  *width *= 0.75 + steps_out / ((double) (1 << (steps_in+2)));


  return x1;
}

double slice_pos_real
  (double (*ll)(double), double width, double left, double x0)
{
  double x1;
  double r;
  double y;
  double lo;
  double right;

  r = genrand_real3();
  y = log(r) + ll(x0);
  lo= getRandomRange(0, width);
  right = x0 - lo + width;
  while(ll(right) > y) right += width;

  for ( x1 = getRandomRange(left, right)
      ; ll(x1) < y
      ; x1 = getRandomRange(left, right)
      )  {
    if (x1 < x0) left = x1; else right = x1;
  }

  return x1;
}

double tune_slice_pos_real
  (double (*ll)(double), double *w, double left, double x0)
{
  double x1;
  double r;
  double y;
  double lo;
  double right;

  int steps_out = 0;
  int steps_in = 0;

  r = genrand_real3();
  y = log(r) + ll(x0);
  lo= getRandomRange(0, *w);

  right = x0 - lo + *w;
  while(ll(right) > y) { right += *w; ++steps_out; }

  for ( x1 = getRandomRange(left, right)
      ; ll(x1) < y
      ; x1 = getRandomRange(left, right)
      )  {
    if (x1 < x0) left = x1; else right = x1;
    ++steps_in;
  }

  /* Trying to optimize the width, 
  assuming an average "step in" cuts the interval in half. 
  Current rule of thumb is to take the weighted average of the current width
  with the estimated ideal width */
  *w *= 0.75 + steps_out / ((double) (1 << (steps_in+2)));

  /* fprintf(stderr, "(%d, [(%f,%d)])\n", VAR, WIDTH(VAR), error); */
  return x1;
}



double slice_real_left_right
  ( double (*ll)(double)
  , double left
  , double right
  , double x0
  )
{
  double x1;
  double r;
  double y;

  r = genrand_real3();
  y = log(r) + ll(x0);

  for ( x1 = getRandomRange(left, right)
      ; ll(x1) < y
      ; x1 = getRandomRange(left, right)
      )
  {
    if (x1 < x0) left = x1; else right = x1;
  }

  return x1;
}

double slice_discrete_right(double (*ll)(double), double n, double x) {
  double y;
  y = floor(getRandomRangeOpenRight(0,n+1));
  return (log(genrand_real3()) < ll(y) - ll(x)) ? y : x;
}

double slice_discrete(double (*ll)(double), double x0)
{
  double y0;
  y0 = ll(x0);

  // x1 is the proposal for the next value
  double x1    = -x0 * log(genrand_real3());
  double y1    = ll(x1);

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
}





