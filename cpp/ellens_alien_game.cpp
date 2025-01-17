namespace targets {

class Alien {
public:
  int x_coordinate;
  int y_coordinate;

  Alien(int x, int y) {
    x_coordinate = x;
    y_coordinate = y;
    health = 3;
  }

  int get_health() { return health; }

  bool hit() {
    if (is_alive()) {
      health--;
    }
    return true;
  }

  bool is_alive() { return health > 0; }

  bool teleport(int x_new, int y_new) {
    x_coordinate = x_new;
    y_coordinate = y_new;
    return true;
  }

  bool collision_detection(Alien other) {
    return other.x_coordinate == x_coordinate &&
           other.y_coordinate == y_coordinate;
  }

private:
  int health;
};

} // namespace targets
