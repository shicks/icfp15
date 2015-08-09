#ifndef AI_H_
#define AI_H_

#include "problem_descriptor.h"
#include "board.h"
#include "misc.h"

class ai {
 public:
  ai(problem_descriptor problem, std::string tag);
  inline ~ai() = default;

  inline ai(const ai &that) = delete;
  inline ai(ai &&that) = delete;
  inline ai &operator=(const ai &that) = delete;
  inline ai &operator=(ai &&that) = delete;

  std::vector<problem_solution> find_solutions();

 private:
  problem_solution find_solution(std::uint32_t seed);
  std::string do_unit(const unit &unit);

  problem_descriptor problem_;
  std::string tag_;
  board board_;
  std::vector<unit> units_;
};

#endif
