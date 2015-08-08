#include "problem_descriptor.h"
#include "board.h"

#include <iostream>
#include <vector>
#include <string>
#include <cstring>
#include <cassert>
#include <limits>
#include <fstream>

char *get_argument(const char *opt, int argc, char *argv[], int *argn) {
  assert(argn);
  ++*argn;
  if (*argn >= argc) {
    std::cerr << opt << " requires an argument" << std::endl;
    std::exit(1);
  } else if (*argv[*argn] == '\0') {
    std::cerr << opt << "  argument cannot be empty" << std::endl;
    std::exit(1);
  }
  return argv[*argn];
}

int main(int argc, char *argv[]) {
  std::vector<std::string> filenames;
  unsigned long timelimit(std::numeric_limits<unsigned long>::max());
  unsigned long max_memory(std::numeric_limits<unsigned long>::max());
  std::vector<std::string> phrases;

  for (int argn(1); argn < argc; ++argn) {
    if (strcmp(argv[argn], "-f") == 0) {
      filenames.emplace_back(get_argument("-f", argc, argv, &argn));
    } else if (strcmp(argv[argn], "-t") == 0) {
      char *endptr;
      timelimit = std::strtoul(get_argument("-t", argc, argv, &argn), &endptr, 10);
      if (*endptr != '\0') {
        std::cerr << "invalid argument to -t" << std::endl;
      }
    } else if (strcmp(argv[argn], "-m") == 0) {
      char *endptr;
      max_memory = std::strtoul(get_argument("-m", argc, argv, &argn), &endptr, 10);
      if (*endptr != '\0') {
        std::cerr << "invalid argument to -t" << std::endl;
      }
    } else if (strcmp(argv[argn], "-p") == 0) {
      phrases.emplace_back(get_argument("-p",argc, argv, &argn));
    } else {
      std::cerr << "unexpected argument: " << argv[argn] << std::endl;
      std::exit(1);
    }
  }
  (void) timelimit;
  (void) max_memory;

  for (const auto &filename : filenames) {
    problem_descriptor problem;
    std::ifstream ifs(filename);
    if (!problem.parse(ifs)) {
      std::cerr << "invalid problem file: " << filename << std::endl;
      std::exit(1);
    }
    board b(problem);
    std::cout << filename << ":" << std::endl;
    std::cout << "width: " << problem.width << ", height: " << problem.height
              << std::endl;
    std::cout << b.to_string() << std::endl;
  }

  return 0;
}
