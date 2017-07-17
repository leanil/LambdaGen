#include "result.hpp"
#include "View.h"
#include <algorithm>
#include <chrono>
#include <iostream>
#include <map>
#include <numeric>
#include <random>
#include <utility>
#include <vector>

using namespace std;

double measure_time(map<string, double*>& bigVectors) {
    double min_time = 999999;
    for (int i = 0; i < 4; ++i) {
        auto start = chrono::high_resolution_clock::now();
        evaluator(bigVectors);
        auto done = chrono::high_resolution_clock::now();
        min_time = min(min_time, chrono::duration_cast<chrono::microseconds>(done - start).count() / 1000000.0);
    }
    return min_time;
}

double* generate_random_data(size_t n) {
    default_random_engine rand;
    uniform_real_distribution<> dist;
    double* data = new double[n];
    generate(data, data + n, [&]() {return dist(rand); });
    return data;
}

double* generate_sequence(int a, int b) {
    double* data = new double[b - a];
    iota(data, data + b - a, a);
    return data;
}

void run_functional_test() {
    map<string, double*> bigVectors{
        { "vec", generate_sequence(7,10)},
        { "mat", generate_sequence(1,7) },
        { "a", generate_sequence(1,4) },
        { "b", generate_sequence(1,4) },
        { "mat8", generate_sequence(1,9) },
        { "tens", generate_sequence(1,25) }
    };
    cout << evaluator(bigVectors) << endl;
}

void run_performance_test() {
    map<string, double*> bigVectors{
        { "vec", generate_random_data(1 << 13) },
        { "mat", generate_random_data(1 << 26) },
        { "a", generate_random_data(100000000)} };
    cout << "time: " << measure_time(bigVectors) << endl;
}

int main() {
    //run_performance_test();
    run_functional_test();
}
