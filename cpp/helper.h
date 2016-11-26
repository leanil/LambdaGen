#pragma once

#include "View.h"
#include <algorithm>
#include <initializer_list>
#include <iterator>
#include <iostream>
#include <numeric>
#include <thread>
#include <type_traits>
#include <vector>

using namespace std;

template<typename T>
vector<T> make_vector(initializer_list<T> list) {
	return vector<T>(list.begin(), list.end());
}

inline size_t batch_size(size_t n, unsigned t) {
	return n / t + (n % t != 0);
}

template<typename F, typename A, typename B, size_t D1, size_t S1, size_t S2, typename... DA, typename... DB>
void Map(const F& f, const View<A, Pair<D1, S1>, DA...>& v, View<B, Pair<D1, S2>, DB...>& result) {
	//transform(v.begin(), v.end(), result.begin(), f);
	for (size_t i = 0; i < D1; ++i) {
		f(v[i])(result[i]);
	}
	//cout << "Map " << v << " = " << result << endl;
}

template<typename F, typename A, typename B, size_t D1, size_t S1, size_t S2, typename... DA, typename... DB>
void ParMap(const F& f, const View<A, Pair<D1, S1>, DA...>& v, View<B, Pair<D1, S2>, DB...>& result, unsigned thread_num) {
	vector<thread> threads;
	threads.reserve(thread_num);
	size_t batch = batch_size(D1, thread_num);
	for (unsigned thread_id = 0; thread_id < thread_num; ++thread_id) {
		threads.push_back(thread([&, thread_id] () {
			for (size_t i = thread_id*batch; i < min((thread_id + 1)*batch, D1); ++i) {
				f(v[i])(result[i], thread_id);
			}
		}));
	}
	for (thread& t : threads) {
		t.join();
	}
	//cout << "ParMap " << v << " = " << result << endl;
}

template<typename F, typename A, typename B, typename D1, typename... DA, typename... DB>
void Reduce(const F& f, const View<A, D1, DA...>& v, View<B, DB...>& result) {
	result.copy(v[0]);
	for (size_t i = 1; i < D1::dim; ++i) {
		f(result)(v[i])(result);
	}
	//cout << "Reduce " << v << " = " << result << endl;
	//return accumulate(v.begin() + 1, v.end(), v.front(), [&](const E& s, const E& e) {return f(s)(e); });
}

template<typename F, typename A, typename B, typename D1, typename... DA, typename... DB>
void ParReduce(const F& f, const View<A, D1, DA...>& v, View<B, DB...>& result, View<B, DB...> temp[], unsigned thread_num) {
	vector<thread> threads;
	threads.reserve(thread_num);
	size_t batch = batch_size(D1::dim, thread_num);
	for (unsigned thread_id = 0; thread_id < thread_num; ++thread_id) {
		threads.push_back(thread([&, thread_id] () {
			if (thread_id*batch < D1::dim) {
				temp[thread_id].copy(v[thread_id*batch]);
			}
			for (size_t i = thread_id*batch + 1; i < min((thread_id + 1)*batch, D1::dim); ++i) {
				f(temp[thread_id])(v[i])(temp[thread_id], thread_id);
			}
		}));
	}
	for (thread& t : threads) {
		t.join();
	}
	result = temp[0];
	for (size_t i = 1; i*batch < D1::dim; ++i) {
		f(result)(temp[i])(result, 0);
	}
	//cout << "ParReduce " << v << " = " << result << endl;
}

template<typename F, typename A, typename B, typename C, size_t D1, size_t S1A, size_t S1B, size_t S1C, typename... DA, typename... DB, typename... DC>
void Zip(const F& f, const View<A, Pair<D1, S1A>, DA...>& a, const View<B, Pair<D1, S1B>, DB...>& b, View<C, Pair<D1, S1C>, DC...>& result) {
	//transform(a.begin(), a.end(), b.begin(), result.begin(), [&](const A& a, const B& b) {return f(a)(b); });
	for (size_t i = 0; i < D1; ++i) {
		f(a[i])(b[i])(result[i]);
	}
	//cout << "Zip " << a << " " << b << " = " << result << endl;
}

template<typename F, typename A, typename B, typename C, size_t D1, size_t S1A, size_t S1B, size_t S1C, typename... DA, typename... DB, typename... DC>
void ParZip(const F& f, const View<A, Pair<D1, S1A>, DA...>& a, const View<B, Pair<D1, S1B>, DB...>& b, View<C, Pair<D1, S1C>, DC...>& result, unsigned thread_num) {
	vector<thread> threads;
	threads.reserve(thread_num);
	size_t batch = batch_size(D1, thread_num);
	for (unsigned thread_id = 0; thread_id < thread_num; ++thread_id) {
		threads.push_back(thread([&, thread_id] () {
			for (size_t i = thread_id*batch; i < min((thread_id + 1)*batch, D1); ++i) {
				f(a[i])(b[i])(result[i], thread_id);
			}
		}));
	}
	for (thread& t : threads) {
		t.join();
	}
	//cout << "ParZip " << a << " " << b << " = " << result << endl;
}
