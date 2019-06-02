#pragma once

#include <numeric>
#include <sstream>
#include <string>

double* gen_seq(int a, int n) {
    double* seq = new double[n];
    std::iota(seq, seq + n, a);
    return seq;
}

template<typename View>
bool check(View const& view, std::string expect) {
    std::stringstream ss;
    ss << view;
    return ss.str() == expect;
}

template<typename View>
bool viewEq(View const& a, View const& b) {
    return a == b;
}

bool viewEq(double a, double b) {
    double abs_err = std::abs(a - b), rel_err = abs_err / a;
    return std::min(abs_err, rel_err) < 1e-6;
}