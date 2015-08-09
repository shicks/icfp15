#ifndef AI_H_
#define AI_H_

#include "problem_descriptor.h"
#include "board.h"
#include "misc.h"
#include "string_matcher.h"

class ai {
 public:
  ai(problem_descriptor problem, std::string tag);
  inline ~ai() = default;

  inline ai(const ai &that) = delete;
  inline ai(ai &&that) = delete;
  inline ai &operator=(const ai &that) = delete;
  inline ai &operator=(ai &&that) = delete;

  void add_phrase_of_power(std::string string) {
    power_table_.add_pattern(std::move(string));
  }

  std::vector<problem_solution> find_solutions();

 private:
  problem_solution find_solution(std::uint32_t seed);
  std::string do_unit(const unit &unit);

  problem_descriptor problem_;
  board board_;
  std::string tag_;
  std::vector<unit> units_;
  string_match_table power_table_;
};

#endif
