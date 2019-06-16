#pragma once

#include "List.h"
#include <algorithm>
#include <cmath>
#include <functional>
#include <iostream>
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>

template<typename... T>
using head = std::tuple_element_t<0, std::tuple<T...>>;

template<int D, int S>
struct P {
    static constexpr int dim = D;
    static constexpr int stride = S;
};

template<int I, int B, typename S> struct Subdiv;

template<int I, int B, typename H, typename T>
struct Subdiv<I, B, List<H, T>> {
    using type = List<H, typename Subdiv<I - 1, B, T>::type>;
};

template<int B, int D, int S, typename T>
struct Subdiv<0, B, List<P<D, S>, T>> {
    using type = List<P<D / B, B*S>, List<P<B, S>, T>>;
};

template<int I, int B, typename S>
using subdiv_t = typename Subdiv<I, B, S>::type;

template<int I, typename S> struct Flatten;

template<int I, typename H, typename T>
struct Flatten<I, List<H, T>> {
    using type = List<H, typename Flatten<I - 1, T>::type>;
};

template<int D1, int S1, int D2, int S2, typename T>
struct Flatten<0, List<P<D1, S1>, List<P<D2, S2>, T>>> {
    using type = List<P<D1*D2, S2>, T>;
};

template<int I, typename S>
using flatten_t = typename Flatten<I, S>::type;

template<int D, typename S> struct Flip;

template<int D, typename H, typename T>
struct Flip<D, List<H, T>> {
    using type = List<H, typename Flip<D - 1, T>::type>;
};

template<typename A, typename B, typename T>
struct Flip<0, List<A, List<B, T>>> {
    using type = List<B, List<A, T>>;
};

template<int D, typename S>
using flip_t = typename Flip<D, S>::type;

template<typename Ptr, typename T, typename Ds, bool IsRef = true> class View;

template<int d, typename Ptr, typename T, typename Ds, bool IsRef>
auto flip(View<Ptr, T, Ds, IsRef> v) {
    return View<Ptr, T, flip_t<d, Ds>, IsRef>(v.data);
}

template<int i, int b, typename Ptr, typename T, typename Ds, bool IsRef>
auto subdiv(View<Ptr, T, Ds, IsRef> v) {
    return View<Ptr, T, subdiv_t<i, b, Ds>, IsRef>(v.data);
}

template<int i, typename Ptr, typename T, typename Ds, bool IsRef>
auto flatten(View<Ptr, T, Ds, IsRef> v) {
    return View<Ptr, T, flatten_t<i, Ds>, IsRef>(v.data);
}

template<typename L>
struct ViewSize {
    template<typename A, typename B>
    struct Prod {
        using type = Int<A::dim * B()>;
    };
    using type = typename Foldr<Prod, Int<1>, L>::type;
};

template<typename V0, typename...>
using size = Int<V0::size>;

template<typename Ptr, typename T, typename D, typename Ds, bool IsRef>
class View<Ptr, T, List<D, Ds>, IsRef> {
public:
    View(Ptr data) : data(data) {}
    View() : data(new T[typename ViewSize<List<D, Ds>>::type()]), share(data) {}
    View (const View&) = default;

    static constexpr int size = D::dim;

    // Omitting this results in an implicitly deleted copy assignment, which takes precedence
    // over the below template.
    View<Ptr, T, List<D, Ds>, IsRef>& operator=(const View<Ptr, T, List<D, Ds>, IsRef>& other) {
        return this->operator=<IsRef>(other);
    }

    template<bool OtherRef>
    View<Ptr, T, List<D, Ds>, IsRef>& operator=(const View<Ptr, T, List<D, Ds>, OtherRef>& other) {
        for (int i = 0; i < D::dim; ++i) {
            (*this)[i] = other[i];
        }
        return *this;
    }

    bool operator==(const View<Ptr, T, List<D, Ds>, IsRef>& other) const {
        for (int i = 0; i < size; ++i) {
            if (!((*this)[i] == other[i]))
                return false;
        }
        return true;
    }

    auto operator[](int idx) const {
        return View<Ptr, T, Ds, true>(data + idx * D::stride);
    }

    //template<typename I, typename... Idx>
    //auto operator()(I i, Idx... idx) const {
    //    return slice<EmptyList, List<D, Ds>>(0, i, idx...);
    //}

    Ptr const data;
    const std::shared_ptr<T> share;
private:
    //template<typename SortedDims, typename Dims, typename I, typename... Idx, std::enable_if_t<std::is_placeholder_v<I> != 0, int> = 0>
    //auto slice(int offset, I i, Idx... idx) const {
    //    return slice<typename Insert<SortedDims, KeyValuePair<Int<std::is_placeholder_v<I>>, typename Dims::head>>::type, typename Dims::tail>(offset, idx...);
    //}

    //template<typename SortedDims, typename Dims, typename I, typename... Idx, std::enable_if_t<std::is_placeholder_v<I> == 0, int> = 0>
    //auto slice(int offset, I i, Idx... idx) const {
    //    return slice<SortedDims, typename Dims::tail>(offset + i * Dims::head::stride, idx...);
    //}

    //template<typename SortedDims, typename Dims>
    //auto slice(int offset) const {
    //    return View<Ptr, T, typename Concat<typename Map<SortedDims, get_value>::type, Dims>::type>(data + offset);
    //}
};

template<typename Ptr, typename T, typename Dims, bool IsRef>
std::ostream& operator<<(std::ostream& out, const View<Ptr, T, Dims, IsRef>& v) {
    out << "{";
    for (int i = 0; i < Dims::head::dim; ++i) {
        out << (i ? "," : "") << v[i];
    }
    return out << "}";
}

template<typename Ptr, typename T>
class View<Ptr, T, EmptyList, true> {
public:
    View() : data(&val) {}
    View(Ptr data) : data(data) {}

    bool operator==(const View<Ptr, T, EmptyList, true>& other) const {
        double abs_err = std::abs(*data - *other.data), rel_err = abs_err / *data;
        return std::min(abs_err, rel_err) < 1e-6;
    }

    View<Ptr, T, EmptyList, true>& operator=(const View<Ptr, T, EmptyList, true>& other) {
        return this->operator=<true>(other);
    }

    template<bool OtherRef>
    View<Ptr, T, EmptyList, true>& operator=(const View<Ptr, T, EmptyList, OtherRef>& other) {
        *data = (T)other;
        return *this;
    }

    void operator=(T x) const {
        *data = x;
    }

    operator T() const {
        return *data;
    }

    Ptr const data;
    T val;
};

// Specialization for holding single scalar partial results / temporaries.
//template<typename Ptr, typename T>
//class View<Ptr, T, EmptyList, false> {
//public:
//
//    template<bool OtherRef>
//    View<Ptr, T, EmptyList, true>& operator=(const View<Ptr, T, EmptyList, OtherRef>& other) {
//        data = (T)other;
//        return *this;
//    }
//
//    void operator=(T x) {
//        data = x;
//    }
//
//    operator T() const {
//        return data;
//    }
//
//    typename T::asd data;
//};

template<typename Ptr, typename T, bool IsRef>
std::ostream& operator<<(std::ostream& out, const View<Ptr, T, EmptyList, IsRef>& v) {
    return out << (T)v;
}
