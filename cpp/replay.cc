#include "misc.h"
#include "board.h"

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstdint>

#include <json/json.h>

int main (int argc, char **argv) {
  if (argc != 3) {
    std::cerr << "usage: " << argv[0] << " problem.json output.json" << std::endl;
    std::exit(1);
  }

  std::string problem_filename(argv[1]);
  std::string solution_filename(argv[2]);

  problem_descriptor problem;
  std::ifstream problem_file(problem_filename);
  if (!problem.parse(problem_file)) {
    std::cerr << "invalid problem file: " << problem_filename << std::endl;
    std::exit(1);
  }

  Json::Reader reader;
  Json::Value solution_array_value;
  std::ifstream solution_file(solution_filename);
  if (!reader.parse(solution_file, solution_array_value, false) ||
      !solution_array_value.isArray()) {
    std::cerr << "invalid output file: " << solution_filename << std::endl;
    std::exit(1);
  }

  for (Json::ArrayIndex n(0); n < solution_array_value.size(); ++n) {
    problem_solution solution;
    if (!solution.from_json_value(solution_array_value[n])) {
      std::cerr << "invalid solution in file: " << solution_filename << std::endl; 
      std::exit(1);
   }
    if (solution.id != problem.id) {
      std::cerr << "solution id " << solution.id
                << " does not match problem id " << problem.id << std::endl;
      std::exit(1);
    }

    board b(problem.width, problem.height);

    std::uint32_t seed(solution.seed);
    integer rnd((seed >> 16) & 0x7fff);
    unit u(unit::from_descriptor(problem.units[rnd % problem.units.size()]));
    integer num_units(0);

    unit_transform xfrm{u.spawn_offset(problem.width), 0};
    if (!b.can_place_unit(u, xfrm)) {
      std::cerr << "can't place initial unit" << std::endl;
      std::exit(1);
    }

    // clear screen
    std::cout << "\e[2J" << std::flush;
    // move to top left
    std::cout << "\e[1;1H" << std::flush;
    std::cout << b.to_string() << std::flush;

    std::size_t step;
    for (step = 0; step < solution.solution.size(); ++step) {
      char c(solution.solution[step]);

      // {p, ', !, ., 0, 3}	move W
      // {b, c, e, f, y, 2}	move E
      // {a, g, h, i, j, 4}	move SW
      // {l, m, n, o, space, 5}    	move SE
      // {d, q, r, v, z, 1}	rotate clockwise
      // {k, s, t, u, w, x}	rotate counter-clockwise
      // \t, \n, \r	(ignored)

      unit_move m;
      switch (c) {
        case 'p': case '\'': case '!': case '.': case '0': case '3':
          m = unit_move::w;
          break;
        case 'b': case 'c': case 'e': case 'f': case 'y': case '2':
          m = unit_move::e;
          break;
        case 'a': case 'g': case 'h': case 'i': case 'j': case '4':
          m = unit_move::sw;
          break;
        case 'l': case 'm': case 'n': case 'o': case ' ': case '5':
          m = unit_move::se;
          break;
        case 'd': case 'q': case 'r': case 'v': case 'z': case '1':
          m = unit_move::cw;
          break;
        case 'k': case 's': case 't': case 'u': case 'w': case 'x':
          m = unit_move::ccw;
          break;
        default:
          std::cerr << "invalid character in solution: " << c << std::endl;
          std::exit(1);
      }

      unit_transform xfrmtmp(xfrm);
      xfrmtmp.apply_move(m);

      board btmp;
      bool locked(!b.can_place_unit(u, xfrmtmp));
      if (locked) {
        b.place_unit(u, xfrm);
        btmp = b;

        seed = source_advance(seed);;
        rnd = (seed >> 16) & 0x7fff;
        u = unit::from_descriptor(problem.units[rnd % problem.units.size()]);
        ++num_units;
        xfrm.offset = u.spawn_offset(problem.width);
        xfrm.ccw_rotation = 0;
      } else {
        btmp = b;
        btmp.place_unit(u, xfrmtmp);
        xfrm = xfrmtmp;
      }

      // move to top left
      std::cout << "\e[1;1H" << std::flush;
      std::cout << btmp.to_string() << std::flush;
      std::cout << "move: " << c << std::endl;
      std::cerr << "press enter to continue" << std::endl;
      while (std::cin.get() != '\n');

    }

    if (step != solution.solution.size()) {
      std::cerr << "too many characters in solution" << std::endl;
    }
    if (num_units > problem.source_length) {
      std::cerr << "too many units" << std::endl;
    }

    std::cerr << "solution: " << solution.solution << std::endl;
    std::cerr << "press enter to continue" << std::endl;
    while (std::cin.get() != '\n');
  }
  
  return 0;
}
