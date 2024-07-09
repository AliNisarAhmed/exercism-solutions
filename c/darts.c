#include "darts.h"
#include <math.h>
#include <stdint.h>

uint8_t score(coordinate_t coord) {
  float distance = hypot(coord.x, coord.y);
  if (distance <= INNER_CIRCLE) {
    return 10;
  } else if (distance <= MIDDLE_CIRCLE) {
    return 5;
  } else if (distance <= OUTER_CIRCLE) {
    return 1;
  } else {
    return 0;
  }
}
