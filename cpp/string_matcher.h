#ifndef STRING_MATCHER_H_
#define STRING_MATCHER_H_

#include "misc.h"

#include <string>
#include <vector>
#include <iostream>

struct string_match_table {

  inline void add_pattern(std::string pattern) {
    std::cerr << "adding pattern: \"" << pattern << "\"" << std::endl;
    kmp.emplace_back(compute_kmp(pattern));
    patterns.emplace_back(std::move(pattern));
    ++num_patterns;
  }

  static std::vector<integer> compute_kmp(const std::string &pattern);

  integer num_patterns;
  std::vector<std::string> patterns;
  std::vector<std::vector<integer>> kmp;

};

struct string_match_state {
  integer q;
  integer num_matches;

  inline bool advance(const std::string &pattern,
                      const std::vector<integer> &kmp,
                      char c) {
    BOOST_ASSERT(0 <= q && q < static_cast<integer>(pattern.size()));
    // integer old_q(q);
    while (q > 0 and pattern[q] != c) q = kmp[q - 1];
    if (pattern[q] == c)
      ++q;
    bool ret(false);
    if (q == static_cast<integer>(pattern.size())) {
      ++num_matches;
      q = kmp[q - 1];
      ret = true;
    }
    BOOST_ASSERT(0 <= q && q < static_cast<integer>(pattern.size()));
    // std::cerr << "advancing \"" << pattern << "\", old q " << old_q << ", new q " << q;
    // if (ret) std::cerr << " (got match)";
    // std::cerr << std::endl;
    return ret;
  }
};

#endif
