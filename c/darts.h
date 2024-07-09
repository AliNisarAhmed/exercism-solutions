#ifndef DARTS_H
#define DARTS_H

#include <stdint.h>

#define INNER_CIRCLE 1.0
#define MIDDLE_CIRCLE 5.0
#define OUTER_CIRCLE 10.0

typedef struct coordinate_t {
  float x;
  float y;
} coordinate_t;

uint8_t score(coordinate_t coord);

#endif
