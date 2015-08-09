#ifndef MISC_H_
#define MISC_H_

#include <boost/assert.hpp>
#include <boost/variant.hpp>

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <type_traits>
#include <vector>
#include <sstream>

// an integer that's big enough
typedef std::int64_t integer;

static constexpr std::size_t big_prime = 4000000133;

static constexpr integer max_board_width = integer(1) << 30;
static constexpr integer max_board_height = integer(1) << 30;

static constexpr std::uint32_t source_multiplier = 1103515245U;
static constexpr std::uint32_t source_increment = 12345U;
static constexpr std::uint32_t source_mask = 0xffffffffU;

static constexpr char solution_tag[] = "c++";

inline constexpr std::uint32_t source_advance(std::uint32_t seed) {
  return ((seed * source_multiplier) + source_increment) & source_mask;
}

// an output iterator that updates a hash
template <typename arg_type_>
class hash_iterator {
 public:
  inline hash_iterator(std::size_t *hash = nullptr) : hash_(hash) {}
  inline hash_iterator(const hash_iterator &that) = default;
  inline hash_iterator(hash_iterator &&that) = default;

  hash_iterator &operator=(const hash_iterator &that) = delete;
  hash_iterator &operator=(hash_iterator &&that) = delete;

  inline hash_iterator &operator=(const arg_type_ &arg) {
    if (hash_) {
      *hash_ *= big_prime;
      *hash_ += arg_hash_(arg);
    }
    return *this;
  }

  inline hash_iterator &operator=(arg_type_ &&arg) {
    if (hash_) {
      *hash_ *= big_prime;
      *hash_ += arg_hash_(std::move(arg));
    }
    return *this;
  }

  inline const hash_iterator &operator=(const arg_type_ &arg) const {
    if (hash_) {
      *hash_ *= big_prime;
      *hash_ += arg_hash_(arg);
    }
    return *this;
  }

  inline const hash_iterator &operator=(arg_type_ &&arg) const {
    if (hash_) {
      *hash_ *= big_prime;
      *hash_ += arg_hash_(std::move(arg));
    }
    return *this;
  }

  inline hash_iterator &operator*() { return *this; }
  inline const hash_iterator &operator*() const { return *this; }
  hash_iterator &operator++() { return *this; }
  hash_iterator &operator++(int) { return *this; }

 private:
  std::hash<arg_type_> arg_hash_;
  std::size_t *hash_;
};

namespace std {
template<typename arg_type_>
struct iterator_traits<::hash_iterator<arg_type_>> {
  typedef void value_type;
  typedef void pointer;
  typedef void reference;
  typedef std::output_iterator_tag iterator_category;
};
}

enum class direction {
  ne,
  nw,
  e,
  w,
  se,
  sw,
};

struct cell_position {
  integer x, y;

  inline bool operator==(const cell_position &that) const {
    return x == that.x && y == that.y;
  }

  inline bool operator!=(const cell_position &that) const {
    return x != that.x || y != that.y;
  }

  inline bool operator<(const cell_position &that) const {
    return x < that.x || (x == that.x && y < that.y);
  }

  inline cell_position operator+(const cell_position &that) const {
    integer a1(x - y / 2);
    integer b1(y);
    integer a2(that.x - that.y / 2);
    integer b2(that.y);
    integer b3(b1 + b2);
    return {a1 + a2 + b3 / 2, b3};
  }

  inline cell_position operator+=(const cell_position &that) {
    *this = *this + that;
    return *this;
  }

  inline cell_position operator-(const cell_position &that) const {
    integer a1(x - y / 2);
    integer b1(y);
    integer a2(that.x - that.y / 2);
    integer b2(that.y);
    integer b3(b1 - b2);
    return {a1 - a2 + b3 / 2, b3};
  }

  inline cell_position operator-=(const cell_position &that) {
    *this = *this - that;
    return *this;
  }

  inline integer distance(const cell_position &that) const {
    std::array<integer, 3> cube1(to_cubic()), cube2(that.to_cubic());
    return ((std::abs(cube1[0] - cube2[0]) + std::abs(cube1[1] - cube2[1]) +
             std::abs(cube1[2] - cube2[2])) /
            2);
  }

  inline void rotate_ccw(int count = 1) {
    count %= 6;
    if (count < 0)
      count += 6;
    // cubical coordinates
    std::array<integer, 3> cube(to_cubic());
    // rotate cube
    std::array<integer, 3> rotated;
    for (integer n(0); n < 3; ++n) {
      rotated[n] = cube[(n + count) % 3];
      if ((count & 1) != 0)
        rotated[n] = -rotated[n];
    }
    // convert back
    from_cubic(rotated);
  }

  inline void rotate_cw(int count = 1) {
    rotate_ccw(-count);
  }

  inline void step(direction dir) {
    switch (dir) {
      case direction::ne:
        if ((y & 1) != 0) ++x;
        --y;
        break;
      case direction::nw:
        if ((y & 1) == 0) --x;
        --y;
        break;
      case direction::e:
        ++x;
        break;
      case direction::w:
        --x;
        break;
      case direction::se:
        if ((y & 1) != 0) ++x;
        ++y;
        break;
      case direction::sw:
        if ((y & 1) == 0) --x;
        ++y;
        break;
    }
  }

  inline std::string to_string() const {
    std::ostringstream oss;
    oss << "(" << x << ", " << y << ")";
    return oss.str();
  }

 private:
  inline std::array<integer, 3> to_cubic() const {
    std::array<integer, 3> cube;
    cube[0] = x - y / 2;
    cube[2] = y;
    cube[1] = -cube[0] - cube[2];
    return cube;
  }

  inline void from_cubic(const std::array<integer, 3> &cube) {
    x = cube[0] + cube[2] / 2;
    y = cube[2];
  }
};

struct unit_descriptor {
  inline bool operator==(const unit_descriptor &that){
    return members == that.members && pivot == that.pivot;
  }

  inline bool operator!=(const unit_descriptor &that){
    return members != that.members || pivot != that.pivot;
  }

  std::vector<cell_position> members;
  cell_position pivot;
};

enum class unit_move : char {
  e,
  w,
  se,
  sw,
  ccw,
  cw,
  first = e,
  last = cw,
};

struct unit_transform {
  cell_position offset;
  integer ccw_rotation;

  inline bool operator==(const unit_transform &that) const {
    BOOST_ASSERT(0 <= ccw_rotation);
    BOOST_ASSERT(ccw_rotation < 6);
    BOOST_ASSERT(0 <= that.ccw_rotation);
    BOOST_ASSERT(that.ccw_rotation < 6);
    return offset == that.offset && ccw_rotation == that.ccw_rotation;
  }

  inline bool operator!=(const unit_transform &that) const {
    BOOST_ASSERT(0 <= ccw_rotation);
    BOOST_ASSERT(ccw_rotation < 6);
    BOOST_ASSERT(0 <= that.ccw_rotation);
    BOOST_ASSERT(that.ccw_rotation < 6);
    return offset != that.offset || ccw_rotation != that.ccw_rotation;
  }

  inline void apply_move(unit_move m) {
    BOOST_ASSERT(0 <= ccw_rotation && ccw_rotation < 6);
    switch (m) {
      case unit_move::e:
          offset.step(direction::e);
        break;
      case unit_move::w:
          offset.step(direction::w);
        break;
      case unit_move::se:
          offset.step(direction::se);
        break;
      case unit_move::sw:
          offset.step(direction::sw);
        break;
      case unit_move::ccw:
        if (ccw_rotation == 5)
          ccw_rotation = 0;
        else
          ++ccw_rotation;
        break;
      case unit_move::cw:
        if (ccw_rotation == 0)
          ccw_rotation = 5;
        else
          --ccw_rotation;
        break;
      default:
        BOOST_ASSERT(false);
    }
  }
};

// pivot == 0, rotations occur around pivot
struct unit {

  static inline unit from_descriptor(const unit_descriptor &descr) {
    unit ret;
    for (const auto &member : descr.members) {
      ret.members.emplace_back(member - descr.pivot);
    }
    std::sort(ret.members.begin(), ret.members.end());
    return ret;
  }

  inline bool operator==(const unit &that) {
    return members == that.members;
  }

  inline bool operator!=(const unit &that) {
    return members != that.members;
  }

  inline void rotate_cw(int count = 1) {
    for (auto &member : members) {
      member.rotate_cw(count);
    }
  }

  inline void rotate_ccw(int count = 1) {
    for (auto &member : members) {
      member.rotate_ccw(count);
    }
  }

  inline void apply_offset(cell_position offset) {
    for (auto &member : members) {
      member += offset;
    }
  }

  inline void apply_transform(unit_transform xfrm) {
    for (auto &member : members) {
      member.rotate_ccw(xfrm.ccw_rotation);
      member += xfrm.offset;
    }
  }

  inline void apply_move(unit_move m) {
    switch (m) {
      case unit_move::e:
        for (auto &member : members)
          member.step(direction::e);
        break;
      case unit_move::w:
        for (auto &member : members)
          member.step(direction::w);
        break;
      case unit_move::se:
        for (auto &member : members)
          member.step(direction::se);
        break;
      case unit_move::sw:
        for (auto &member : members)
          member.step(direction::sw);
        break;
      case unit_move::cw:
        rotate_cw(1);
        break;
      case unit_move::ccw:
        rotate_ccw(1);
        break;
    }
  }

  inline cell_position spawn_offset(integer board_width) const {
    integer y(0);
    integer x_left(board_width - 1), x_right(0);
    for (const auto &member : members) {
      x_left = std::min(x_left, member.x);
      x_right = std::max(x_right, member.x);
      y = std::max(y, -member.y);
    }
    integer x(((board_width - 1) - x_right - x_left) / 2);
    return {x, y};
  }

  std::string to_string() const {
    std::ostringstream oss;
    oss << "[";
    bool first(true);
    for (const auto &member : members) {
      if (!first) {
        oss << ", ";
      }
      first = false;
      oss << "(" << member.x << ", " << member.y << ")";
    }
    oss << "]";
    return oss.str();
  }

  std::vector<cell_position> members;
};

struct path {
  std::string text;
  std::vector<unit_move> moves;
  integer score;
  unit_transform end;
};

namespace std {
template<>
struct hash<::cell_position> {
  inline std::size_t operator()(const ::cell_position &cell) const {
    return integer_hash_(cell.x) * big_prime + integer_hash_(cell.y);
  }

 private:
  hash<integer> integer_hash_;
};

template<>
struct hash<::unit_transform> {
  inline std::size_t operator()(const ::unit_transform &xfrm) const {
    return cell_position_hash_(xfrm.offset) * big_prime + integer_hash_(xfrm.ccw_rotation);
  }

 private:
  hash<cell_position> cell_position_hash_;
  hash<integer> integer_hash_;
};

template <>
struct hash<::unit_descriptor> {
  inline std::size_t operator()(const unit_descriptor &unit) const {
    std::size_t ret(0);
    for (const auto &member : unit.members) {
      ret *= big_prime;
      ret += cell_position_hash_(member);
    }
    ret *= big_prime;
    ret += cell_position_hash_(unit.pivot);
    return ret;
  }

 private:
  const hash<::cell_position> cell_position_hash_;
};

template <>
struct hash<::unit> {
  inline std::size_t operator()(const unit_descriptor &unit) const {
    std::size_t ret(0);
    for (const auto &member : unit.members) {
      ret *= big_prime;
      ret += cell_position_hash_(member);
    }
    return ret;
  }

 private:
  const hash<::cell_position> cell_position_hash_;
};
}

#endif
