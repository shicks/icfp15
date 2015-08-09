#ifndef PROBLEM_DESCRIPTOR_H_
#define PROBLEM_DESCRIPTOR_H_

#include "misc.h"

#include <json/json.h>

#include <stdexcept>
#include <cstdint>
#include <iostream>
#include <string>
#include <vector>

struct problem_descriptor {
  bool parse(std::istream &is) {
    Json::Reader reader;
    Json::Value root;
    if (!reader.parse(is, root, false)) {
      return false;
    }
    return from_json_value(root);
  }

  bool parse(const std::string &str) {
    Json::Reader reader;
    Json::Value root;
    if (!reader.parse(str, root, false)) {
      return false;
    }
    return from_json_value(root);
  }

  bool from_json_value(const Json::Value &root);

  integer id;
  std::vector<unit_descriptor> units;
  integer width;
  integer height;
  std::vector<cell_position> filled;
  integer source_length;
  std::vector<std::uint32_t> source_seeds;
};

struct problem_solution {
  integer id;
  std::uint32_t seed;
  std::string tag;
  std::string solution;

  Json::Value to_json_value() const;
  std::string to_string() const;
};

#endif
