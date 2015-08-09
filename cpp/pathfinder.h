#ifndef PATHFINDER_H_
#define PATHFINDER_H_

#include "board.h"
#include "misc.h"
#include "string_matcher.h"

#include <queue>
#include <string>
#include <unordered_map>
#include <unordered_set>

class pathfinder {
  // state for cells we can move into
  struct path_cache_entry {
    unit_transform previous_xfrm;
    integer start_distance;
    integer path_score;
    unit_move last_move;
    char last_letter;
    std::vector<string_match_state> string_match;
  };
  typedef std::unordered_map<unit_transform, path_cache_entry> path_cache_type;

  // state for cells we can lock into
  struct locking_cache_entry {
    unit_move locking_move;
    integer locking_score;
    char locking_letter;
  };
  typedef std::unordered_map<unit_transform, locking_cache_entry> locking_cache_type;

  static constexpr integer row_fill_score = 1000;
  static constexpr integer cell_stacking_score = 2000;
  static constexpr integer move_down_score = 100;
  static constexpr integer power_char_score = 50;
  static constexpr integer rotate_score = -10;
  static constexpr integer move_ew_score = -10;

  struct frontier_queue_entry {
    unit_transform xfrm;
    integer start_distance;
    integer path_score;

    bool operator<(const frontier_queue_entry &that) const {
      return path_score < that.path_score;
    }
  };
  typedef std::priority_queue<frontier_queue_entry> frontier_queue_type;

 public:
  pathfinder(const board *new_board,
             const unit *new_unit,
             const string_match_table *power_table_);
  ~pathfinder() = default;

  pathfinder() = delete;
  pathfinder(const pathfinder &that) = delete;
  pathfinder(pathfinder &&that) = delete;

  pathfinder &operator=(const pathfinder &that) = delete;
  pathfinder &operator=(pathfinder &&that) = delete;

  path find_best_path(unit_transform start_xfrm);

 private:
  integer get_path_score(const unit_transform &xfrm,
                         unit_move math,
                         const path_cache_entry &current_path_cache_entry,
                         char *letter);
  integer get_cell_locking_score(const unit_transform &xfrm);

  // find new_xfrm in predecessors of xfrm
  inline bool find_cycle(unit_transform xfrm,
                         const unit_transform &new_xfrm,
                         const path_cache_type &cache) {
    while (xfrm != new_xfrm) {
      //std::cerr << "finding cycle " << xfrm.offset.to_string() << " rotation " << xfrm.ccw_rotation << std::endl;
      auto cache_iter(cache.find(xfrm));
      BOOST_ASSERT(cache_iter != cache.end());
      const auto &cell_state(cache_iter->second);
      if (cell_state.start_distance == 0)
        return false; // this is the start point
      xfrm = cell_state.previous_xfrm;
    }
    return true;
  }

  const board *board_;
  const unit *unit_;
  unit_transform start_xfrm_;
  std::array<unit, 6> ccw_rotated_unit_;
  std::array<std::unordered_set<cell_position>, 6> ccw_rotated_unit_members_;
  integer symmetry_;
  const string_match_table *power_table_;
};

#endif
