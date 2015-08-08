#include "ai.h"
#include "pathfinder.h"

#include <queue>
#include <unordered_map>
#include <iostream>
#include <string>
#include <chrono>
#include <vector>

ai::ai(problem_descriptor problem)
    : problem_(std::move(problem)), board_(problem_.width, problem_.height) {
  board_.fill(problem_.filled);
  for (const auto &unit_descr : problem_.units) {
    units_.emplace_back(unit::from_descriptor(unit_descr));
  }
}

std::vector<problem_solution> ai::find_solutions() {
  std::cerr << "finding solution for problem " << problem_.id << std::endl;
  std::vector<problem_solution> ret;
  for (std::uint32_t seed : problem_.source_seeds) {
    ret.emplace_back(find_solution(seed));
  }
  return ret;
}

problem_solution ai::find_solution(std::uint32_t seed) {
  std::cerr << "finding solution for seed " << seed << std::endl;
  problem_solution sol;
  std::string solution_text;

  for (integer n(0); n < problem_.source_length; ++n) {
    unit *current_unit(&units_[seed % units_.size()]);
    std::string unit_text(do_unit(*current_unit));
    if (unit_text.empty())
      break;
    solution_text.append(unit_text);
    seed = source_advance(seed);
  }

  sol.id = problem_.id;
  sol.seed = seed;
  sol.tag.append(solution_tag);
  sol.tag.push_back('@');
  sol.tag.append(
      std::to_string(std::chrono::duration_cast<std::chrono::seconds>(
                         std::chrono::system_clock::now().time_since_epoch())
                         .count()));
  sol.solution = std::move(solution_text);

  std::cerr << "solution:" << std::endl << sol.to_string() << std::endl;
  return sol;
}

std::string ai::do_unit(const unit &unit) {
  std::cerr << "finding solution for unit: " << unit.to_string() << std::endl;

  pathfinder pf(&board_, &unit);

  unit_transform spawn_xfrm{unit.spawn_offset(board_.width()), 0};
  path best_path(pf.find_best_path(spawn_xfrm));

  std::cerr << "path: \"" << best_path.text << "\"" << std::endl;

  if (best_path.moves.empty())
    return "";

  std::cerr << "placing unit at " << best_path.end.offset.to_string()
            << " rotation " << best_path.end.ccw_rotation << std::endl;
  board_.place_unit(unit, best_path.end);

  std::cerr << "new board: " << std::endl << board_.to_string() << std::endl;

  return std::move(best_path.text);
}
