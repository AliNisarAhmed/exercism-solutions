#include <array>
#include <cstdio>
#include <string>
#include <vector>

// Round down all provided student scores.
std::vector<int> round_down_scores(std::vector<double> student_scores) {
  std::vector<int> result{};
  for (int i = 0; i < student_scores.size(); i++) {
    result.emplace_back(static_cast<int>(student_scores[i]));
  }
  return result;
}

// Count the number of failing students out of the group provided.
int count_failed_students(std::vector<int> student_scores) {
  int count = 0;
  for (int i = 0; i < student_scores.size(); i++) {
    if (student_scores[i] <= 40) {
      count++;
    }
  }
  return count;
}

// Determine how many of the provided student scores were 'the best' based on
// the provided threshold.
std::vector<int> above_threshold(std::vector<int> student_scores,
                                 int threshold) {
  std::vector<int> result{};
  for (int i = 0; i < student_scores.size(); i++) {
    if (student_scores[i] >= threshold) {
      result.emplace_back(student_scores[i]);
    }
  }
  return result;
}

// Create a list of grade thresholds based on the provided highest grade.
std::array<int, 4> letter_grades(int highest_score) {
  std::array<int, 4> grades{};
  int interval = (highest_score - 40) / 4;
  int grade = 41;
  for (int i = 0; i < 4; i++) {
    grades[i] = grade;
    grade += interval;
  }
  return grades;
}

// Organize the student's rank, name, and grade information in ascending order.
std::vector<std::string>
student_ranking(std::vector<int> student_scores,
                std::vector<std::string> student_names) {
  std::vector<std::string> result{};
  for (int i = 0; i < student_scores.size(); i++) {
    result.emplace_back("" + std::to_string(i + 1) + ". " + student_names[i] +
                        ": " + std::to_string(student_scores[i]));
  }
  return result;
}

// Create a string that contains the name of the first student to make a perfect
// score on the exam.
std::string perfect_score(std::vector<int> student_scores,
                          std::vector<std::string> student_names) {
  for (int i = 0; i < student_scores.size(); i++) {
    if (student_scores[i] == 100) {
      return student_names[i];
    }
  }
  return "";
}
