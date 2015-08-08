#include "board.h"

std::string board::to_string() {
  // 2 columns per cell, left & right borders and newline
  const int ret_width(2 * width_ + 3);
  // 1 row per cell, top & bottom borders
  const int ret_height(height_ + 2);
  std::string ret(ret_width * ret_height, ' ');

  for (int col(1); col < 1 + 2*width_; ++col) {
    ret[col] = '-';
    ret[ret_width * (1 + height_) + col] = '-';
  }
  for (int row(1); row < 1 + height_; ++row) {
    ret[row * ret_width] = '|';
    ret[row * ret_width + 1 + 2*width_] = '|';
    ret[row * ret_width + 1 + 2*width_ + 1] = '\n';
  }
  ret[0] = '/';
  ret[1 + 2*width_] = '\\';
  ret[1 + 2*width_ + 1] = '\n';
  ret[ret_width * (1 + height_)] = '\\';
  ret[ret_width * (1 + height_) + 1 + 2 * width_] = '/';
  ret[ret_width * (1 + height_) + 1 + 2 * width_ + 1] = '\n';

  for (int y(0); y < height_; ++y) {
    for (int x(0); x < width_; ++x) {
      int row(1 + y);
      int col(1 + (y & 1) + 2 * x);
      ret[ret_width * row + col] = test({x, y}) ? '*' : '.';
    }
  }

  return ret;
}
