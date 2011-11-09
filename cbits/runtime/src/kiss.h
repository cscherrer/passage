/*****************************************************************************/
/* Implementation of a 32-bit KISS generator which uses no multiply instructions */

extern unsigned int rx, ry, rz, rw, rc;
#pragma omp threadprivate (rx,ry,rz,rw,rc)

static inline
void init_genrand(unsigned long seed) {
  rx = seed;
  ry = seed + 1;
  rz = seed + 2;
  rw = seed + 3;
  rc = seed & 1;
}

static inline
unsigned long genrand_int32(void) {

  int t;
  ry ^= ry << 5;
  ry ^= ry >> 7;
  ry ^= ry << 22;
  t = rz + rw + rc;
  rz = rw;
  rc = t < 0;
  rw = t & 2147483647;
  rx += 1411392427;
  return rx + ry + rw;
}





