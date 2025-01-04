#pragma once

#include <string>

namespace star_map {

enum class System {
  BetaHydri,
  Sol,
  EpsilonEridani,
  AlphaCentauri,
  DeltaEridani,
  Omicron2Eridani
};

}

namespace heaven {

class Vessel {
public:
  star_map::System current_system;
  int generation;
  std::string name;
  int busters = 0;

  Vessel(std::string name, int generation,
         star_map::System system = star_map::System::Sol);

  Vessel replicate(std::string name);

  void make_buster();
  bool shoot_buster();
};

std::string get_older_bob(Vessel first, Vessel second);
bool in_the_same_system(Vessel first, Vessel second);

} // namespace heaven
