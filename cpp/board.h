#ifndef BOARD_H_
#define BOARD_H_

#include "misc.h"
#include "problem_descriptor.h"

#include <boost/assert.hpp>
#include <boost/dynamic_bitset.hpp>

#include <cassert>

class board {
 public:
  board() : width_(0), height_(0), cells_(), num_set_(0) {}

  board(integer width, integer height)
      : width_(width), height_(height), cells_(width * height) {}

  board(const board &that)
      : width_(that.width_), height_(that.height_), cells_(that.cells_) {}

  board(board &&that)
      : width_(that.width_),
        height_(that.height_),
        cells_(std::move(that.cells_)) {}

  board(const problem_descriptor &problem)
      : board(problem.width, problem.height) {
    fill(problem.filled);
  }

  inline integer width() const { return width_; }
  inline integer height() const { return height_; }

  inline void resize(integer width, integer height) {
    BOOST_ASSERT(width_ > 0);
    BOOST_ASSERT(width_ <= max_board_width);
    BOOST_ASSERT(height_ > 0);
    BOOST_ASSERT(height_ <= max_board_width);
    cells_.reset();
    cells_.resize(width_ * height_);
    num_set_ = 0;
  }

  inline void clear() {
    resize(0, 0);
  }

  inline bool empty() {
    return num_set_ == 0;
  }

  void fill(const std::vector<cell_position> &cells) {
    for (const auto &cell : cells) {
      set(cell);
    }
  }

  inline bool test(cell_position cell) const {
    BOOST_ASSERT(cell.x >= 0);
    BOOST_ASSERT(cell.y >= 0);
    BOOST_ASSERT(cell.x < width_);
    BOOST_ASSERT(cell.y < height_);
    return cells_.test(cell.y * width_ + cell.x);
  }

  inline void set(cell_position cell, bool val = true) {
    BOOST_ASSERT(cell.x >= 0);
    BOOST_ASSERT(cell.y >= 0);
    BOOST_ASSERT(cell.x < width_);
    BOOST_ASSERT(cell.y < height_);
    auto n(cell.y * width_ + cell.x);
    if (!cells_.test(n)) {
      ++num_set_;
      cells_.set(n, val);
    }
  }

  inline void reset(cell_position cell) {
    BOOST_ASSERT(cell.x >= 0);
    BOOST_ASSERT(cell.y >= 0);
    BOOST_ASSERT(cell.x < width_);
    BOOST_ASSERT(cell.y < height_);
    auto n(cell.y * width_ + cell.x);
    if (cells_.test(n)) {
      --num_set_;
      cells_.reset(n);
    }
  }

  inline bool can_place_unit(const unit &unit) const {
    for (const auto &member : unit.members) {
      if (member.x < 0 || member.x >= width_)
        return false;
      if (member.y < 0 || member.y >= height_)
        return false;
      if (test(member))
        return false;
    }
    return true;
  }

  inline bool can_place_unit(const unit &unit, unit_transform xfrm) const {
    for (const auto &member : unit.members) {
      cell_position xfrm_pos(member);
      xfrm_pos.rotate_ccw(xfrm.ccw_rotation);
      xfrm_pos += xfrm.offset;
      if (xfrm_pos.x < 0 || xfrm_pos.x >= width_)
        return false;
      if (xfrm_pos.y < 0 || xfrm_pos.y >= height_)
        return false;
      if (test(member))
        return false;
    }
    return true;
  }

  void place_unit(const unit &unit) {
    BOOST_ASSERT(can_place_unit(unit));
    for (auto member : unit.members) {
      set(member);
    }
  }

  std::string to_string();

  std::size_t hash() const {
    std::size_t ret(0);
    hash_iterator<boost::dynamic_bitset<>::block_type> i(&ret);
    boost::to_block_range(cells_, i);
    return ret;
  }

 private:
  integer width_, height_;
  boost::dynamic_bitset<> cells_;
  integer num_set_;
};

namespace std {
template <>
struct hash<board> {
  std::size_t operator()(const board &b) const { return b.hash(); }
};
}

#endif
