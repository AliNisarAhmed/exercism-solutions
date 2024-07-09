#include "hamming.h"

int compute(const char *lhs, const char *rhs) {
  if (!lhs || !rhs) {
    return -1;
  }

  int count = 0;
  for (; *lhs != '\0' && *rhs != '\0'; lhs++, rhs++) {
    if (*lhs != *rhs) {
      count++;
    }
  }

  if (*lhs || *rhs) {
    return -1;
  }

  return count;
}
