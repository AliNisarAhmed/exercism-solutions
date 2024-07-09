#include "resistor_color.h"
static resistor_band_t color_array[10] = {COLORS};

int color_code(resistor_band_t band) { return band; }

resistor_band_t *colors() { return color_array; }
