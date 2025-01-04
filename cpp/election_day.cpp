#include <string>
#include <vector>

namespace election {

// The election result struct is already created for you:

struct ElectionResult {
  // Name of the candidate
  std::string name{};
  // Number of votes the candidate has
  int votes{};
};

// vote_count takes a reference to an `ElectionResult` as an argument and will
// return the number of votes in the `ElectionResult.
int vote_count(ElectionResult &result) { return result.votes; }

// increment_vote_count takes a reference to an `ElectionResult` as an argument
// and a number of votes (int), and will increment the `ElectionResult` by that
// number of votes.
void increment_vote_count(ElectionResult &result, int num_votes) {
  result.votes += num_votes;
}

// determine_result receives the reference to a final_count and returns a
// reference to the `ElectionResult` of the new president. It also changes the
// name of the winner by prefixing it with "President". The final count is given
// in the form of a `reference` to `std::vector<ElectionResult>`, a vector with
// `ElectionResults` of all the participating candidates.
ElectionResult &determine_result(std::vector<ElectionResult> &results) {
  int winner_index = 0;
  int max = 0;
  for (int i = 0; i < results.size(); i++) {
    if (results[i].votes > max) {
      max = results[i].votes;
      winner_index = i;
    }
  }
  ElectionResult &winner = results[winner_index];
  winner.name = "President " + winner.name;
  return winner;

  // from: https://exercism.org/tracks/cpp/exercises/election-day/solutions/siebenschlaefer
  // auto &result =
  //     *std::max_element(std::begin(results), end(results),
  //                       [](auto &a, auto &b) { return a.votes < b.votes; });
  // result.name.insert(0, "President ");
  // return result;
}

} // namespace election
