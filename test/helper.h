#pragma once

#include "View.h"
#include <CL/sycl.hpp>
#include <algorithm>
#include <initializer_list>
#include <iterator>
#include <iostream>
#include <numeric>
#include <thread>
#include <type_traits>
#include <vector>

using namespace std;

cl::sycl::handler* act_cgh;

inline size_t batch_size(size_t n, unsigned t) {
	return n / t + (n % t != 0);
}

template<typename F, typename Ptr, typename A, typename B, size_t D1, size_t... DA, size_t... DB>
void Map(F f, View<Ptr, A, D1, DA...> v, View<Ptr, B, D1, DB...> result) {
	for (size_t i = 0; i < D1; ++i) {
		f(v[i])(result[i]);
	}
	//cout << "Map " << v << " = " << result << endl;
}

template<typename F, typename Ptr, typename A, typename B, size_t D1, size_t... DA, size_t... DB>
void ParMap(F f, View<Ptr, A, D1, DA...> v, View<Ptr, B, D1, DB...> result, unsigned thread_num) {
	size_t batch = batch_size(D1, thread_num);
	act_cgh->parallel_for<class MapKernel>(cl::sycl::range<1>{thread_num}, [=] (cl::sycl::id<1> idx) mutable {
    for (size_t i = idx.get(0)*batch; i < min((idx.get(0) + 1)*batch, D1); ++i) {
      f(v[i])(result[i], idx.get(0));
    }
  });
	//cout << "ParMap " << v << " = " << result << endl;
}

template<typename F, typename Ptr, typename A, typename B, size_t D1, size_t... DA, size_t... DB>
void Reduce(const F& f, View<Ptr, A, D1, DA...> v, View<Ptr, B, DB...> result) {
	result = v[0];
	for (size_t i = 1; i < D1; ++i) {
		f(result)(v[i])(result);
	}
	//cout << "Reduce " << v << " = " << result << endl;
}

template<typename F, typename Ptr, typename A, typename B, size_t D1, size_t Tmp, size_t... DA, size_t... DB>
void ParReduce(F f, View<Ptr, A, D1, DA...> v, View<Ptr, B, DB...> result, View<Ptr, B, Tmp, DB...> temp, unsigned thread_num) {
	size_t batch = batch_size(D1, thread_num);
	act_cgh->parallel_for<class ReduceKernel1>(cl::sycl::range<1>{thread_num}, [=] (cl::sycl::id<1> idx) mutable {
   if (idx.get(0)*batch < D1) {
     temp[idx.get(0)] = v[idx.get(0)*batch];
   }
   for (size_t i = idx.get(0)*batch + 1; i < min((idx.get(0) + 1)*batch, D1); ++i) {
     f(temp[idx.get(0)])(v[i])(temp[idx.get(0)], idx.get(0));
   }
	});
	//cout << "ParReduce " << v << " = " << result << endl;
}

template<typename F, typename Ptr, typename A, typename B, size_t D1, size_t Tmp, size_t... DA, size_t... DB>
void ParReduceJoin(F f, View<Ptr, A, D1, DA...> v, View<Ptr, B, DB...> result, View<Ptr, B, Tmp, DB...> temp, unsigned thread_num) {
  size_t batch = batch_size(D1, thread_num);
  act_cgh->single_task<class ReduceKernel2>([=] () mutable {
    result = temp[0];
    for (size_t i = 1; i*batch < D1; ++i) {
      f(result)(temp[i])(result, 0);
    }
  });
}

template<typename F, typename Ptr, typename A, typename B, typename C, size_t D1, size_t... DA, size_t... DB, size_t... DC>
void Zip(F f, View<Ptr, A, D1, DA...> a, View<Ptr, B, D1, DB...> b, View<Ptr, C, D1, DC...> result) {
	for (size_t i = 0; i < D1; ++i) {
		f(a[i])(b[i])(result[i]);
	}
	//cout << "Zip " << a << " " << b << " = " << result << "\n";
}

template<typename F, typename Ptr, typename A, typename B, typename C, size_t D1, size_t... DA, size_t... DB, size_t... DC>
void ParZip(F f, View<Ptr, A, D1, DA...> a, View<Ptr, B, D1, DB...> b, View<Ptr, C, D1, DC...> result, unsigned thread_num) {
	size_t batch = batch_size(D1, thread_num);
	act_cgh->parallel_for<class ZipKernel>(cl::sycl::range<1>{thread_num}, [=] (cl::sycl::id<1> idx) mutable {
   for (size_t i = idx.get(0)*batch; i < min((idx.get(0) + 1)*batch, D1); ++i) {
     f(a[i])(b[i])(result[i], idx.get(0));
   }
	});
	//cout << "ParZip " << a << " " << b << " = " << result << endl;
}
