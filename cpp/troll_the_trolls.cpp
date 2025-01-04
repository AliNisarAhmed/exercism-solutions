namespace hellmath {

enum class AccountStatus { troll, guest, user, mod };

enum class Action { read, write, remove };

bool display_post(AccountStatus poster, AccountStatus viewer) {
  return poster != AccountStatus::troll || viewer == AccountStatus::troll;
}

bool permission_check(Action action, AccountStatus st) {
  switch (st) {
  case AccountStatus::guest:
    return action == Action::read;
  case AccountStatus::user:
  case AccountStatus::troll:
    return action == Action::read || action == Action::write;
  case AccountStatus::mod:
    return true;
  default:
    return false;
  }
}

bool valid_player_combination(AccountStatus player1, AccountStatus player2) {
  if (player1 == AccountStatus::guest || player2 == AccountStatus::guest) {
    return false;
  }
  bool player1_is_troll = player1 == AccountStatus::troll;
  bool player2_is_troll = player2 == AccountStatus::troll;
  if ((player1_is_troll || player2_is_troll) &&
      !(player1_is_troll && player2_is_troll)) {
    return false;
  }
  return true;
}

bool has_priority(AccountStatus status1, AccountStatus status2) {
  switch (status1) {
  case AccountStatus::mod:
    return status2 != AccountStatus::mod;
  case AccountStatus::user:
    return status2 == AccountStatus::guest || status2 == AccountStatus::troll;
  case AccountStatus::guest:
    return status2 == AccountStatus::troll;
  default:
    return false;
  }
}

} // namespace hellmath
