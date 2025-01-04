#include "doctor_data.h"
#include <string>

heaven::Vessel::Vessel(std::string name, int generation,
                       star_map::System system) {
  this->name = name;
  this->generation = generation;
  this->current_system = system;
}

heaven::Vessel heaven::Vessel::replicate(std::string new_name) {
  return heaven::Vessel(new_name, this->generation + 1, this->current_system);
}

void heaven::Vessel::make_buster() { this->busters++; }

bool heaven::Vessel::shoot_buster() {
  if (this->busters > 0) {
    this->busters--;
    return true;
  }
  return false;
}

std::string heaven::get_older_bob(heaven::Vessel first, heaven::Vessel second) {
  if (first.generation < second.generation) {
    return first.name;
  }
  return second.name;
}

bool heaven::in_the_same_system(heaven::Vessel first, heaven::Vessel second) {
  return first.current_system == second.current_system;
}
