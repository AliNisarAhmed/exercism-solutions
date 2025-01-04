#include <string>

namespace log_line {
std::string message(std::string line) {
  int index = line.find(':');
  return line.substr(index + 2);
}

std::string log_level(std::string line) {
  // return the log level
  int start_index = line.find('[');
  int end_index = line.find(']');
  return line.substr(start_index + 1, end_index - 1);
}

std::string reformat(std::string line) {
  return message(line) + " " + "(" + log_level(line) + ")";
}
} // namespace log_line
