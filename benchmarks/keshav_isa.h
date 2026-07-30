#ifndef KESHAV_ISA_H
#define KESHAV_ISA_H

#include <stdint.h>

/**
  * Keshav-ISA Macro Header
  * Provides hardware-optimized inline operations for dual-issue scalar execution.
  */

#define KESHAV_SADD(dest, a, b) dest = (a) + (b)
#define KESHAV_MIN(dest, a, b)  dest = ((a) < (b)) ? (a) : (b)
#define KESHAV_MAX(dest, a, b)  dest = ((a) > (b)) ? (a) : (b)

#endif // KESHAV_ISA_H
