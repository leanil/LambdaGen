#pragma once

#include <algorithm>
#include <array>
#include <iostream>
#include <iterator>

template<typename Ptr, typename T, size_t... Dims> struct View {};

template<typename Ptr, typename T, size_t D1, size_t... Dims>
struct View<Ptr, T, D1, Dims...> {

    using stride_t = std::array<size_t, sizeof...(Dims)+1>;

    View(stride_t stride, const Ptr& data, size_t base = 0) : stride(stride), data(data), base(base) {}

    struct View<Ptr, T, D1, Dims...>& operator=(const View<Ptr, T, D1, Dims...>& other) {
         for (size_t i = 0; i < D1; ++i) {
             (*this)[i] = other[i];
         }
         return *this;
    }

    View<Ptr, T, Dims...> operator[](size_t idx) const {
        std::array<size_t, sizeof...(Dims)> new_stride;
        std::copy(std::next(stride.begin()), stride.end(), new_stride.begin());
        return View<Ptr, T, Dims...>(new_stride, data, base + idx*stride[0]);
    }

    stride_t stride;
    Ptr data;
    size_t base;
};

template<typename Ptr, typename T, size_t D1, size_t... Dims>
std::ostream& operator<<(std::ostream& out, const View<Ptr, T, D1, Dims...>& v) {
    out << "{";
    for (size_t i = 0; i < D1; ++i) {
        out << (i ? "," : "") << v[i];
    }
    return out << "}";
}


template<typename Ptr, typename T>
struct View<Ptr, T> {

    View(std::array<size_t, 0>, Ptr data, size_t base = 0) : data(data), base(base) {}

    // Accessor index type should be cl::sycl::id (according to 3.4.6.3 of the spec), but it works only with integer;
    View<Ptr, T>& operator=(const View<Ptr, T>& other) {
        data[base] = other.data[other.base];
        return *this;
    }

    void operator=(T x) {
        data[base] = x;
    }

    operator T() const {
        return data[base];
    }

    Ptr data;
    size_t base;
};

template<typename Ptr, typename T>
std::ostream& operator<<(std::ostream& out, const View<Ptr, T>& v) {
    return out << (T)v;
}
