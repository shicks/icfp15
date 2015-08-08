#ifndef AI_H_
#define AI_H_

#include "problem_descriptor.h"
#include "board.h"
#include "misc.h"

class ai {
  ai(problem_descriptor problem);
  inline ~ai() = default;

  inline ai(const ai &that) = delete;
  inline ai(ai &&that) = delete;
  inline ai &operator=(const ai &that) = delete;
  inline ai &operator=(ai &&that) = delete;

 private:
  problem_descriptor problem_;
  board board_;
};

#endif
