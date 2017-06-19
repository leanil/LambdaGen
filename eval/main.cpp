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

int main() {
	double mat[]{1,2,3,4,5,6}, vec[]{7,8,9};
	map<string, double*> bigVectors{
		{ "mat", mat },
		{ "vec", vec }
	};
	cout << evaluator(bigVectors) << endl;
}
