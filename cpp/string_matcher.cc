#include "string_matcher.h"
#include "misc.h"

std::vector<integer> string_match_table::compute_kmp(const std::string &pattern) {
  std::vector<integer> ret(pattern.size(), 0);
  integer k(0);
  for (integer q(1); q < static_cast<integer>(pattern.size()); ++q) {
    while (k > 0 && pattern[k] != pattern[q]) {
      k = ret[k];
    }
    if (pattern[k] == pattern[q])
      ++k;
    ret[q] = k;
  }
  return ret;
}
