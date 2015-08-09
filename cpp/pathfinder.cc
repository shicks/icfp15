#include "pathfinder.h"
#include "misc.h"

#include <boost/assert.hpp>

#include <queue>
#include <unordered_set>

namespace {
constexpr integer row_fill_score = 100;
constexpr integer cell_stacking_score = 10;
constexpr integer move_down_score = 10;

struct frontier_queue_entry {
  unit_transform xfrm;
  integer start_distance;
  integer path_score;

  bool operator<(const frontier_queue_entry &that) const {
    return path_score < that.path_score;
  }
};
typedef std::priority_queue<frontier_queue_entry> frontier_queue_type;
}

pathfinder::pathfinder(const board *new_board, const unit *new_unit)
    : board_(new_board),
      unit_(new_unit),
      start_xfrm_{new_unit->spawn_offset(board_->width()), 0},
      ccw_rotated_unit_{
          *new_unit, *new_unit, *new_unit, *new_unit, *new_unit, *new_unit},
      ccw_rotated_unit_members_{},
      symmetry_(6) {
  BOOST_ASSERT(board_ != nullptr);
  for (integer n(0); n < 6; ++n) {
    ccw_rotated_unit_[n].rotate_ccw(n);
    std::sort(ccw_rotated_unit_[n].members.begin(), ccw_rotated_unit_[n].members.end());
    for (const auto &member : ccw_rotated_unit_[n].members) {
      ccw_rotated_unit_members_[n].emplace(member);
    }
  }
  // find symmetry, defaults to 6.  only need to try factors of 6
  for (integer symmetry : {1, 2, 3}) {
    if (ccw_rotated_unit_[0] == ccw_rotated_unit_[symmetry]) {
      symmetry_ = symmetry;
      break;
    }
  }
  // std::cerr << "symmetry: " << symmetry_ << std::endl;
}

integer pathfinder::get_move_base_score(const unit_transform &xfrm,
                                        unit_move move,
                                        char *letter) {
  // todo
  BOOST_ASSERT(letter != nullptr);
  switch (move) {
    case unit_move::e:
      *letter = 'b';
      break;
    case unit_move::w:
      *letter = 'p';
      break;
    case unit_move::se:
      *letter = 'l';
      break;
    case unit_move::sw:
      *letter = 'a';
      break;
    case unit_move::cw:
      *letter = 'd';
      break;
    case unit_move::ccw:
      *letter = 'k';
      break;
    default:
      BOOST_ASSERT(false);
  }
  return 0;
}

integer pathfinder::get_move_path_score(const unit_transform &xfrm,
                                        unit_move move,
                                        const unit_transform &new_xfrm,
                                        const path_cache_type &cache) {
  integer ret(0);
  switch (move) {
    case unit_move::se:
    case unit_move::sw:
      ret += move_down_score * new_xfrm.offset.y;
      break;
    default:
      break;
  }
  return ret;
}

integer pathfinder::get_cell_locking_score(const unit_transform &xfrm) {
  integer ret(0);
  integer new_row_occupancy[board_->height()];
  unit xfrm_unit(ccw_rotated_unit_[xfrm.ccw_rotation]);
  xfrm_unit.apply_offset(xfrm.offset);
  BOOST_ASSERT(board_->can_place_unit(xfrm_unit));

  // find completed rows
  for (integer y(0); y < board_->height(); ++y) {
    new_row_occupancy[y] = board_->row_occupancy(y);
  }
  for (const auto &member : xfrm_unit.members) {
    BOOST_ASSERT(0 <= member.y && member.y < board_->height());
    ret += row_fill_score * new_row_occupancy[member.y] *
           new_row_occupancy[member.y];
    ++new_row_occupancy[member.y];
  }

  // points for each row completed
  integer rows_completed(0);
  for (integer y(0); y < board_->height(); ++y) {
    if (new_row_occupancy[y] == board_->width()) {
      // give a higher score to rows higher up
      ++rows_completed;
      ret += row_fill_score * board_->width() * board_->width() *
             (board_->height() - y) / board_->height();
    }
  }

  if (rows_completed == 0) {
    for (const auto &member : xfrm_unit.members) {
      cell_position sw(member);
      sw.step(direction::sw);
      if (sw.x < 0 || sw.x >= board_->width() || sw.y < 0 ||
          sw.y >= board_->height() || board_->test(sw)) {
        // std::cerr << "stacking " << member.to_string() << " on " << sw.to_string() << std::endl;
        ret += cell_stacking_score;
      }
      cell_position se(member);
      se.step(direction::se);
      if (se.x < 0 || se.x >= board_->width() || se.y < 0 ||
          se.y >= board_->height() || board_->test(se)) {
        // std::cerr << "stacking " << member.to_string() << " on " << se.to_string() << std::endl;
        ret += cell_stacking_score;
      }
    }
  }
  return ret;
}

// something like A*, but maximizes a score instead of minimizing distance
path pathfinder::find_best_path(unit_transform start_xfrm) {
  if (!board_->can_place_unit(*unit_, {start_xfrm.offset, start_xfrm.ccw_rotation % symmetry_})) {
    return path{"", {}, -1};
  }

  frontier_queue_type frontier_queue;
  frontier_queue.push(frontier_queue_entry{start_xfrm, 0, 0});
  path_cache_type path_cache{
      {start_xfrm, path_cache_entry{start_xfrm, 0, 0}},
  };
  locking_cache_type locking_cache;

  bool have_best_score(false);
  unit_transform best_xfrm;
  integer best_score;

  while (!frontier_queue.empty()) {
    auto current(frontier_queue.top());
    frontier_queue.pop();
    // std::cerr << "checking cell " << current.xfrm.offset.to_string()
    //           << " rotation " << current.xfrm.ccw_rotation << " path score "
    //           << current.path_score << std::endl;
    BOOST_ASSERT(0 <= current.xfrm.ccw_rotation);
    BOOST_ASSERT(current.xfrm.ccw_rotation < symmetry_);
    auto current_path_cache_iter(path_cache.find(current.xfrm));
    BOOST_ASSERT(current_path_cache_iter != path_cache.end());
    auto current_path_cache_entry(current_path_cache_iter->second);

    unit_move m(unit_move::first);
    while (true) {
      unit_transform new_xfrm(current.xfrm);
      new_xfrm.apply_move(m);
      new_xfrm.ccw_rotation %= symmetry_;

      const unit &rotated_unit(ccw_rotated_unit_[new_xfrm.ccw_rotation]);
      char letter;
      integer new_base_score(get_move_base_score(current.xfrm, m, &letter));
      if (board_->can_place_unit(rotated_unit, new_xfrm.offset)) {
        auto new_path_score(
            current.path_score + new_base_score +
            get_move_path_score(
                current.xfrm, m, new_xfrm, path_cache));
        path_cache_entry new_path_cache_entry{
            current.xfrm, current.start_distance + 1, new_path_score, m, letter};
        auto p(path_cache.emplace(new_xfrm, new_path_cache_entry));
        bool visit(false);
        if (p.second) {
          // std::cerr << "added new path with score " << p.first->second.path_score << " to "
          //           << p.first->first.offset.to_string() << " rotation "
          //           << p.first->first.ccw_rotation << " from "
          //           << current.xfrm.offset.to_string() << " rotation "
          //           << current.xfrm.ccw_rotation << std::endl;
          visit = true;
        } else {
          if (new_path_score > p.first->second.path_score) {
            if (!find_cycle(current.xfrm, new_xfrm, path_cache)) {
              // std::cerr << "overwriting old path with score "
              //           << p.first->second.path_score << " to "
              //           << new_xfrm.offset.to_string() << " rotation "
              //           << new_xfrm.ccw_rotation << " to new score "
              //           << new_path_score << " from "
              //           << current.xfrm.offset.to_string() << " rotation "
              //           << current.xfrm.ccw_rotation << std::endl;
              p.first->second = new_path_cache_entry;
              visit = true;
            }
          }
        }
        if (visit) {
          // std::cerr << "queuing " << new_xfrm.offset.to_string() << " rotation "
          //           << new_xfrm.ccw_rotation << std::endl;
          frontier_queue.push(frontier_queue_entry{
              new_xfrm, current.start_distance + 1, new_path_score});
        }
      } else {
        auto locking_cache_iter(locking_cache.find(current.xfrm));
        integer new_locking_score(new_base_score + get_cell_locking_score(current.xfrm));
        if (locking_cache_iter == locking_cache.end()) {
          locking_cache_iter =
              locking_cache.emplace(current.xfrm,
                                    locking_cache_entry{m, new_locking_score, letter})
                  .first;
        } else {
          if (new_locking_score > locking_cache_iter->second.locking_score) {
            locking_cache_iter->second.locking_score = new_locking_score;
            locking_cache_iter->second.locking_letter = letter;
          }
        }
        integer new_score(current_path_cache_entry.path_score +
                          locking_cache_iter->second.locking_score);
        // std::cerr << "potential locking point " << current.xfrm.offset.to_string()
        //           << " rotation " << current.xfrm.ccw_rotation << " locking score "
        //           << new_locking_score << " final score " << new_score << std::endl;
        if (!have_best_score || new_score > best_score) {
          best_score = new_score;
          best_xfrm = current.xfrm;
          have_best_score = true;
        }
      }
      if (m == unit_move::last) {
        break;
      }
      m = static_cast<unit_move>(static_cast<char>(m) + 1);
    }
  }
  BOOST_ASSERT(have_best_score);

  // std::cerr << "best ending location: " << best_xfrm.offset.to_string()
  //           << " rotation " << best_xfrm.ccw_rotation << " score " << best_score
  //           << std::endl;

  path ret;
  auto path_cache_iter(path_cache.find(best_xfrm));
  auto locking_cache_iter(locking_cache.find(best_xfrm));
 
  BOOST_ASSERT(path_cache_iter != path_cache.end());
  BOOST_ASSERT(locking_cache_iter != locking_cache.end());
  ret.score = path_cache_iter->second.path_score + locking_cache_iter->second.locking_score;
  ret.moves.emplace_back(locking_cache_iter->second.locking_move);
  ret.text.push_back(locking_cache_iter->second.locking_letter);
  while (path_cache_iter->second.start_distance > 0) {
    ret.moves.emplace_back(path_cache_iter->second.last_move);
    ret.text.push_back(path_cache_iter->second.last_letter);
    path_cache_iter = path_cache.find(path_cache_iter->second.previous_xfrm);
    BOOST_ASSERT(path_cache_iter != path_cache.end());
  }
  std::reverse(ret.moves.begin(), ret.moves.end());
  std::reverse(ret.text.begin(), ret.text.end());
  ret.end = best_xfrm;

  return ret;
}
