#include "pathfinder.h"
#include "misc.h"

#include <boost/assert.hpp>

#include <queue>

namespace {
struct frontier_queue_entry {
  unit_transform xfrm;
  integer start_distance;
  integer score;

  bool operator<(const frontier_queue_entry &that) const {
    return score > that.score;
  }
};
typedef std::priority_queue<frontier_queue_entry> frontier_queue_type;
}

// something like A*, but maximizes a score instead of minimizing distance
path pathfinder::find_best_path(unit_transform start_xfrm) {
  if (!board_->can_place_unit(*unit_, start_xfrm)) {
    return path{{}, -1};
  }

  board closed[6];
  frontier_queue_type frontier_queue;
  frontier_queue.push(frontier_queue_entry{start_xfrm, 0, 0});
  cache_type cache{
      {start_xfrm, cell_state{start_xfrm, 0, 0}},
  };

  integer best_score(0);
  unit_transform best_xfrm(start_xfrm);

  while (!frontier_queue.empty()) {
    auto current(frontier_queue.top());
    frontier_queue.pop();
    BOOST_ASSERT(0 <= current.xfrm.ccw_rotation);
    BOOST_ASSERT(current.xfrm.ccw_rotation < 6);
    auto current_cache_iter(cache.find(current.xfrm));
    BOOST_ASSERT(current_cache_iter != cache.end());
    auto current_cache(current_cache_iter->second);

    unit_move m(unit_move::first);
    while (true) {
      
      unit_transform new_xfrm(current.xfrm);
      new_xfrm.apply_move(m);

      if (board_.can_place_unit(unit_, new_xfrm)) {
        auto new_score(current.score +
                       score_move(current.xfrm, m, new_xfrm, cache));
        cell_state new_state{
            new_xfrm, current.start_distance + 1, new_score, m};
        auto p(cache.emplace(new_xfrm, new_state));
        bool visit(false);
        if (p.second) {
          visit = true;
        } else {
          if (new_state.score > p.first->second.score) {
            if (!find_cycle(current.xfrm, new_xfrm, cache)) {
              p.first->second = new_state;
              visit = true;
            }
          }
        }
        if (visit) {
          frontier_queue.push(frontier_queue_entry{start_xfrm, 0, 0});
        }
      }
      if (m == unit_move::last) {
        break;
      }
      m = static_cast<unit_move>(static_cast<char>(m) + 1);
    }
  }

  path ret;
  auto path_cache_iter(cache.find(best_score

  return ret;
}
