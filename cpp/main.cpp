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

pair<double, double> measure_time(map<string, double*>& bigVectors) {
	double min_total_time = 999999, min_calc_time = 999999;
	for (int i = 0; i < 4; ++i) {
		auto start = chrono::high_resolution_clock::now();
		//min_calc_time = min(min_calc_time, evaluator(bigVectors));
		auto done = chrono::high_resolution_clock::now();
		min_total_time = min(min_total_time, chrono::duration_cast<chrono::milliseconds>(done - start).count() / 1000.0);
	}
	return{ min_total_time, min_calc_time };
}

double* generate_random_data(size_t n) {
	default_random_engine rand;
	uniform_real_distribution<> dist;
	double* data = new double[n];
	generate(data, data + n, [&] () {return dist(rand); });
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
		{ "vec", generate_random_data(10000000) },
		{ "mat", generate_random_data(160000000) },
		{ "a", generate_random_data(100000000)} };
	auto result = measure_time(bigVectors);
	cout << "total time   calc time\n"
		<< result.first << "      " << result.second << endl;
}

int main() {
	run_functional_test();
}
