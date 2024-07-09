#include "grains.h"
#include <math.h>

uint64_t square(uint8_t index) {
  if (index < 1 || index > 64) {
    return 0;
  }
  return pow(2, index - 1);
}

uint64_t total(void) { return ((((uint64_t)1 << 63) - 1) << 1) + 1; }

// https://exercism.org/tracks/c/exercises/grains/approaches/bit-shifting
