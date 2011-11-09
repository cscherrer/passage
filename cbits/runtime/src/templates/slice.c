#if !(defined(LEFT) && defined(RIGHT))
static double WIDTH(VAR) = 1;
#endif

/* Slice over the reals, or a sub-range of the reals. */
/* static inline */
double SLICE(VAR)(double x0)
{
  double x1;
  double r;
  double y;
#if !(defined(LEFT) && defined(RIGHT))
  double lo;
#endif
  double left;
  double right;

  r = genrand_real3();
  y = log(r) + LL_FUN(VAR)(x0);
#if !(defined(LEFT) && defined(RIGHT))
  lo= getRandomRange(0, WIDTH(VAR));
#endif

#if defined(LEFT)
  left = LEFT;
#else
  left = x0 - lo;
  while(LL_FUN(VAR)(left) > y) left -= WIDTH(VAR);
#endif

#if defined(RIGHT)
  right = RIGHT;
#else
  right = x0 - lo + WIDTH(VAR);
  while(LL_FUN(VAR)(right) > y) right += WIDTH(VAR);
#endif

  for ( x1 = getRandomRange(left, right)
      ; LL_FUN(VAR)(x1) < y
      ; x1 = getRandomRange(left, right)
      )  {
    if (x1 < x0) left = x1; else right = x1;
  }

  return x1;
}


#if !(defined(LEFT) && defined(RIGHT))

/* Slice over the reals */
/* static inline */
double SLICE_TUNE(VAR)(double x0)
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
  y = log(r) + LL_FUN(VAR)(x0);
  lo= getRandomRange(0, WIDTH(VAR));

#if defined(LEFT)
  left = LEFT;
#else
  left = x0 - lo;
  while(LL_FUN(VAR)(left) > y)  { left -= WIDTH(VAR); ++steps_out; }
#endif

#if defined(RIGHT)
  right = RIGHT;
#else
  right = x0 - lo + WIDTH(VAR);
  while(LL_FUN(VAR)(right) > y) { right += WIDTH(VAR); ++steps_out; }
#endif

  for ( x1 = getRandomRange(left, right)
      ; LL_FUN(VAR)(x1) < y
      ; x1 = getRandomRange(left, right)
      )  {
    if (x1 < x0) left = x1; else right = x1;
    ++steps_in;
  }

  /* Trying to optimize the width, 
  assuming an average "step in" cuts the interval in half. 
  Current rule of thumb is to take the weighted average of the current width
  with the estimated ideal width */
  WIDTH(VAR) *= 0.75 + steps_out / ((double) (1 << (steps_in+2)));

  /* fprintf(stderr, "(%d, [(%f,%d)])\n", VAR, WIDTH(VAR), error); */
  return x1;
}

#endif
