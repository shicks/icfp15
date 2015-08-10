#include "problem_descriptor.h"
#include "board.h"
#include "ai.h"

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
  std::string tag;
  std::string output;

  for (int argn(1); argn < argc; ++argn) {
    if (strcmp(argv[argn], "-f") == 0) {
      filenames.emplace_back(get_argument("-f", argc, argv, &argn));
    } else if (strcmp(argv[argn], "-t") == 0) {
      char *endptr;
      timelimit = std::strtoul(get_argument("-t", argc, argv, &argn), &endptr, 10);
      if (*endptr != '\0') {
        std::cerr << "invalid argument to -t" << std::endl;
        std::exit(1);
      }
    } else if (strcmp(argv[argn], "-m") == 0) {
      char *endptr;
      max_memory = std::strtoul(get_argument("-m", argc, argv, &argn), &endptr, 10);
      if (*endptr != '\0') {
        std::cerr << "invalid argument to -t" << std::endl;
        std::exit(1);
      }
    } else if (strcmp(argv[argn], "-p") == 0) {
      phrases.emplace_back(get_argument("-p",argc, argv, &argn));
    } else if (strcmp(argv[argn], "-i") == 0) {
      tag = get_argument("-i",argc,argv,&argn);
    } else if (strcmp(argv[argn], "-o") == 0) {
      output = get_argument("-o",argc,argv,&argn);
    } else {
      std::cerr << "unexpected argument: " << argv[argn] << std::endl;
      std::exit(1);
    }
  }
  (void) timelimit;
  (void) max_memory;

  Json::Value all_solutions(Json::arrayValue);
  for (const auto &filename : filenames) {
    problem_descriptor problem;
    std::ifstream ifs(filename);
    if (!problem.parse(ifs)) {
      std::cerr << "invalid problem file: " << filename << std::endl;
      std::exit(1);
    }
    board b(problem);
    std::cerr << filename << ":" << std::endl;
    std::cerr << "width: " << problem.width << ", height: " << problem.height
              << std::endl;
    std::cerr << b.to_string() << std::endl;
    ai solver(problem, tag);
    for (const auto &phrase : phrases) {
      solver.add_phrase_of_power(phrase);
    }
    auto solutions(solver.find_solutions());
    for (const auto &solution : solutions) {
      all_solutions.append(solution.to_json_value());
    }
  }

  std::ofstream of;
  std::ostream *os;
  if (!output.empty()) {
    of.open(output);
    if (!of) {
      std::cerr << "failed to open output file " << output << std::endl;
      std::exit(1);
    }
    os = &of;
  } else {
    os = &std::cout;
  }
  *os << Json::StyledWriter().write(all_solutions) << std::endl;

  return 0;
}
