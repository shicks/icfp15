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
static constexpr char source_seeds_mname[] = "sourceSeeds";
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
  const Json::Value *x_value(
      value.find(cell_x_name, cell_x_name + (sizeof cell_x_name - 1)));
  if (!int_from_json_value(*x_value, 0, width - 1, &cell->x)) return false;
  const Json::Value *y_value(
      value.find(cell_y_name, cell_y_name + (sizeof cell_y_name - 1)));
  if (!int_from_json_value(*y_value, 0, height - 1, &cell->y)) return false;
  if (cell->y >= height) return false;
  return true;
}

bool unit_descriptor_from_json_value(const Json::Value &value,
                                     integer width,
                                     integer height,
                                     unit_descriptor *unit) {
  BOOST_ASSERT(unit != nullptr);
  if (!value.isObject()) return false;

  const Json::Value *pivot_value(value.find(
      unit_pivot_name, unit_pivot_name + (sizeof unit_pivot_name - 1)));
  if (!pivot_value ||
      !cell_position_from_json_value(*pivot_value, width, height, &unit->pivot))
    return false;

  const Json::Value *members_value(value.find(
      unit_members_name, unit_members_name + (sizeof unit_members_name - 1)));
  if (!members_value || !members_value->isArray()) return false;
  for (Json::ArrayIndex member_num(0); member_num < members_value->size();
       ++member_num) {
    unit->members.emplace_back();
    if (!cell_position_from_json_value(
            (*members_value)[member_num], width, height, &unit->members.back()))
      return false;
  }
  if (unit->members.empty())
    return false;

  return true;
}
}

bool problem_descriptor::from_json_value(const Json::Value &root_value) {
  if (!root_value.isObject()) return false;

  const Json::Value *id_value(
      root_value.find(id_name, id_name + (sizeof id_name - 1)));
  if (!id_value ||
      !int_from_json_value(*id_value,
                           std::numeric_limits<integer>::min(),
                           std::numeric_limits<integer>::max(),
                           &id))
    return false;

  const Json::Value *width_value(
      root_value.find(width_name, width_name + (sizeof width_name - 1)));
  if (!width_value ||
      !int_from_json_value(*width_value, 1, max_board_width, &width))
    return false;

  const Json::Value *height_value(
      root_value.find(height_name, height_name + (sizeof height_name - 1)));
  if (!height_value ||
      !int_from_json_value(*height_value, 1, max_board_height, &height))
    return false;

  const Json::Value *units_value(
      root_value.find(units_name, units_name + (sizeof units_name - 1)));
  if (!units_value || !units_value->isArray()) return false;
  for (Json::ArrayIndex unit_num(0); unit_num < units_value->size(); ++unit_num) {
    units.emplace_back();
    if (!unit_descriptor_from_json_value(
            (*units_value)[unit_num], width, height, &units.back()))
      return false;
  }

  const Json::Value *filled_value(
      root_value.find(filled_name, filled_name + (sizeof filled_name - 1)));
  if (!filled_value || !filled_value->isArray()) return false;
  for (Json::ArrayIndex cell_num(0); cell_num < filled_value->size();
       ++cell_num) {
    filled.emplace_back();
    if (!cell_position_from_json_value(
            (*filled_value)[cell_num], width, height, &filled.back()))
      return false;
  }

  const Json::Value *source_length_value(
      root_value.find(source_length_name,
                      source_length_name + (sizeof source_length_name - 1)));
  if (!source_length_value ||
      !int_from_json_value(*source_length_value,
                           0,
                           std::numeric_limits<integer>::max(),
                           &source_length))
    return false;

  const Json::Value *source_seeds_value(
      root_value.find(source_seeds_mname,
                      source_seeds_mname + (sizeof source_seeds_mname - 1)));
  if (!source_seeds_value || !source_seeds_value->isArray()) return false;
  for (Json::ArrayIndex seed_num(0); seed_num < source_seeds_value->size();
       ++seed_num) {
    source_seeds.emplace_back();
    if (!uint_from_json_value((*source_seeds_value)[seed_num],
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

void problem_solution::to_json_value(Json::Value *value) {
  (*value)[problem_id_name] = Json::LargestInt(id);
  (*value)[seed_name] = Json::LargestUInt(seed);
  (*value)[tag_name] = tag;
  (*value)[solution_name] = solution;
}

std::string problem_solution::to_string() {
  Json::Value value;
  to_json_value(&value);
  Json::StyledWriter writer;
  return writer.write(value);
}
