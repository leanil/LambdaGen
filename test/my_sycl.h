#pragma once

#include "View.h"
#include <CL/sycl.hpp>

using accessor = cl::sycl::accessor<double, 1, cl::sycl::access::mode::read_write, cl::sycl::access::target::global_buffer>;
using buffer_t = cl::sycl::buffer<double, 1>;

const auto rw_access = cl::sycl::access::mode::read_write;
const auto atomic_access = cl::sycl::access::mode::atomic;

// template<typename... Dims>
// cl::sycl::range<1> linearize() {
  // return cl::sycl::range<1>{Size<Dims...>::result};
// }

// namespace cl {
// namespace sycl {

// template<typename Ptr, typename T>
// inline const stream& stream_value(const stream& os, const View<Ptr, T>& v) {
  // return os << (T)v;
// }

// template<typename Ptr, typename T, typename D1, typename... Dims>
// inline const stream& stream_value(const stream& os, const View<Ptr, T, D1, Dims...>& v) {
  // os << "{";
	// for (size_t i = 0; i < D1::dim; ++i) {
		// os << (i ? "," : "") << v[i];
	// }
	// return os << "}";
// }

// }
// }