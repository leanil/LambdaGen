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
    return ss.str() != expect;
}