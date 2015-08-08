#ifndef PATHFINDER_H_
#define PATHFINDER_H_

#include "board.h"
#include "misc.h"

#include <string>
#include <unordered_map>

class pathfinder {
  struct cell_state {
    unit_transform previous_xfrm;
    integer start_distance;
    integer score;
    unit_move last_move;
  };
  typedef std::unordered_map<unit_transform, cell_state> cache_type;

 public:
  inline pathfinder(const board *new_board, const unit *new_unit)
      : board_(new_board),
        unit_(new_unit),
        start_xfrm_{new_unit->spawn_offset(board_->width()), 0},
        ccw_rotated_unit_{
            *new_unit, *new_unit, *new_unit, *new_unit, *new_unit, *new_unit} {
    BOOST_ASSERT(board_ != nullptr);
    for (integer n(0); n < 6; ++n) {
      ccw_rotated_unit_[n].rotate_ccw(n);
    }
  }
  ~pathfinder() = default;

  pathfinder() = delete;
  pathfinder(const pathfinder &that) = delete;
  pathfinder(pathfinder &&that) = delete;

  pathfinder &operator=(const pathfinder &that) = delete;
  pathfinder &operator=(pathfinder &&that) = delete;

  void add_phrase_of_power(std::string string) {
    power_.emplace_back(std::move(string));
  }

  path find_best_path(unit_transform start_xfrm);

 private:
  integer score_move(const unit_transform &xfrm,
                     unit_move move,
                     const unit_transform &new_xfrm,
                     const cache_type &cache);
  // find new_xfrm in predecessors of xfrm
  inline bool find_cycle(unit_transform xfrm,
                         const unit_transform &new_xfrm,
                         const cache_type &cache) {
    while (true) {
      auto cache_iter(cache.find(xfrm));
      BOOST_ASSERT(cache_iter != cache.end());
      const auto &cell_state(cache_iter->second);
      if (cell_state.start_distance == 0)
        return false; // this is the start point
      if (move == unit_move::sw ||
          move == unit_move::se)
        return false;
      xfrm = cell_state.previous_xfrm;
      if (xfrm == new_xfrm)
        return true;
    }
  }

  const board *board_;
  const unit *unit_;
  unit_transform start_xfrm_;
  std::array<unit, 6> ccw_rotated_unit_;
  std::vector<std::string> power_;
};

#endif
