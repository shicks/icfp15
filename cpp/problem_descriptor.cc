#include "problem_descriptor.h"

#include <boost/assert.hpp>
#include <boost/mpl/identity.hpp>
#include <limits>

static constexpr char id_name[] = "id";
static constexpr char units_name[] = "units";
static constexpr char unit_members_name[] = "members";
static constexpr char unit_pivot_name[] = "pivot";
static constexpr char width_name[] = "width";
static constexpr char height_name[] = "height";
static constexpr char filled_name[] = "filled";
static constexpr char source_length_name[] = "sourceLength";
static constexpr char source_seeds_name[] = "sourceSeeds";
static constexpr char cell_x_name[] = "x";
static constexpr char cell_y_name[] = "y";

namespace {

template <typename Int>
bool int_from_json_value(const Json::Value &value,
                         typename boost::mpl::identity<Int>::type min,
                         typename boost::mpl::identity<Int>::type max,
                         Int *n) {
  BOOST_ASSERT(n != nullptr);
  if (!value.isInt()) return false;
  Json::LargestInt t(value.asLargestInt());
  if (t < min || t > max) return false;
  *n = t;
  return true;
}

template <typename UInt>
bool uint_from_json_value(const Json::Value &value,
                          typename boost::mpl::identity<UInt>::type min,
                          typename boost::mpl::identity<UInt>::type max,
                          UInt *n) {
  BOOST_ASSERT(n != nullptr);
  if (!value.isUInt()) return false;
  Json::LargestUInt t(value.asLargestUInt());
  if (t < min || t > max) return false;
  *n = t;
  return true;
}

bool cell_position_from_json_value(const Json::Value &value,
                                   integer width,
                                   integer height,
                                   cell_position *cell) {
  BOOST_ASSERT(cell != nullptr);
  if (!value.isObject()) return false;
  if (!value.isMember(cell_x_name) || !int_from_json_value(value[cell_x_name], 0, width - 1, &cell->x))
    return false;
  if (!value.isMember(cell_y_name) || !int_from_json_value(value[cell_y_name], 0, height - 1, &cell->y)) return false;
  if (cell->y >= height) return false;
  return true;
}

bool unit_descriptor_from_json_value(const Json::Value &value,
                                     integer width,
                                     integer height,
                                     unit_descriptor *unit) {
  BOOST_ASSERT(unit != nullptr);
  if (!value.isObject()) return false;

  if (!value.isMember(unit_pivot_name) ||
      !cell_position_from_json_value(value[unit_pivot_name], width, height, &unit->pivot))
    return false;

  if (!value.isMember(unit_members_name) || !value[unit_members_name].isArray()) return false;
  for (Json::ArrayIndex member_num(0); member_num < value[unit_members_name].size();
       ++member_num) {
    unit->members.emplace_back();
    if (!cell_position_from_json_value(value[unit_members_name][member_num],
                                       width,
                                       height,
                                       &unit->members.back()))
      return false;
  }
  if (unit->members.empty())
    return false;

  return true;
}
}

bool problem_descriptor::from_json_value(const Json::Value &root_value) {
  if (!root_value.isObject()) return false;

  if (!root_value.isMember(id_name) ||
      !int_from_json_value(root_value[id_name],
                           std::numeric_limits<integer>::min(),
                           std::numeric_limits<integer>::max(),
                           &id))
    return false;

  if (!root_value.isMember(width_name) ||
      !int_from_json_value(root_value[width_name], 1, max_board_width, &width))
    return false;

  if (!root_value.isMember(height_name) ||
      !int_from_json_value(root_value[height_name], 1, max_board_height, &height))
    return false;

  if (!root_value.isMember(units_name) || !root_value[units_name].isArray())
    return false;
  for (Json::ArrayIndex unit_num(0); unit_num < root_value[units_name].size(); ++unit_num) {
    units.emplace_back();
    if (!unit_descriptor_from_json_value(
            root_value[units_name][unit_num], width, height, &units.back()))
      return false;
  }

  if (!root_value.isMember(filled_name) || !root_value[filled_name].isArray()) return false;
  for (Json::ArrayIndex cell_num(0); cell_num < root_value[filled_name].size();
       ++cell_num) {
    filled.emplace_back();
    if (!cell_position_from_json_value(
            root_value[filled_name][cell_num], width, height, &filled.back()))
      return false;
  }

  if (!root_value.isMember(source_length_name) ||
      !int_from_json_value(root_value[source_length_name],
                           0,
                           std::numeric_limits<integer>::max(),
                           &source_length))
    return false;

  if (!root_value.isMember(source_seeds_name) || !root_value[source_seeds_name].isArray()) return false;
  for (Json::ArrayIndex seed_num(0); seed_num < root_value[source_seeds_name].size();
       ++seed_num) {
    source_seeds.emplace_back();
    if (!uint_from_json_value(root_value[source_seeds_name][seed_num],
                              std::numeric_limits<std::uint32_t>::min(),
                              std::numeric_limits<std::uint32_t>::max(),
                              &source_seeds.back()))
      return false;
  }

  if (units.empty())
    return false;
  if (source_seeds.empty())
    return false;

  return true;
}

static constexpr char problem_id_name[] = "problemId";
static constexpr char seed_name[] = "seed";
static constexpr char tag_name[] = "tag";
static constexpr char solution_name[] = "solution";

bool problem_solution::from_json_value(const Json::Value &root_value) {
  BOOST_ASSERT(unit != nullptr);
  if (!root_value.isObject()) return false;

  if (!root_value.isMember(problem_id_name) ||
      !int_from_json_value(root_value[problem_id_name],
                           std::numeric_limits<integer>::min(),
                           std::numeric_limits<integer>::max(),
                           &id))
    return false;
  std::cerr << "problem_id: " << id << std::endl;

  if (!root_value.isMember(seed_name) ||
      !uint_from_json_value(root_value[seed_name],
                            std::numeric_limits<std::uint32_t>::min(),
                            std::numeric_limits<std::uint32_t>::max(),
                            &seed))
      return false;
  std::cerr << "seed: " << seed << std::endl;

  if (!root_value.isMember(tag_name) ||
      !root_value[tag_name].isString())
      return false;
  tag = root_value[tag_name].asString();
  std::cerr << "tag: " << tag << std::endl;

  if (!root_value.isMember(solution_name) ||
      !root_value[solution_name].isString())
      return false;
  solution = root_value[solution_name].asString();
  std::cerr << "solution: " << solution << std::endl;

  return true;
}

Json::Value problem_solution::to_json_value() const {
  Json::Value value(Json::objectValue);
  value[problem_id_name] = Json::LargestInt(id);
  value[seed_name] = Json::LargestUInt(seed);
  value[tag_name] = tag;
  value[solution_name] = solution;
  return value;
}

std::string problem_solution::to_string() const {
  return Json::StyledWriter().write(to_json_value());
}
