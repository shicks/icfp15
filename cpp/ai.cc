#include "ai.h"

#include <queue>
#include <unordered_map>

ai::ai(problem_descriptor problem)
    : problem_(std::move(problem)), board_(problem_.width, problem_.height) {
}
