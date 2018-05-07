#pragma once

#include "List.h"
#include <functional>
#include <iostream>
#include <memory>
#include <type_traits>
#include <utility>

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
    using type = List<P<D/B, B*S>, List<P<B, S>, T>>;
};

template<int I, int B, typename S>
using subdiv_t = typename Subdiv<I, B, S>::type;

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

template<typename Ptr, typename T, typename Ds> class View;

template<int d, typename Ptr, typename T, typename Ds>
auto flip(View<Ptr, T, Ds> v) {
    return View<Ptr, T, flip_t<d, Ds>>(v.data);
}

template<int i, int b, typename Ptr, typename T, typename Ds>
auto subdiv(View<Ptr, T, Ds> v) {
    return View<Ptr, T, subdiv_t<i, b, Ds>>(v.data);
}

template<typename L>
struct ViewSize {
    template<typename A, typename B>
    struct Prod {
        using type = Int<A::dim * B()>;
    };
    using type = typename Foldr<Prod, Int<1>, L>::type;
};

template<typename Ptr, typename T, typename D, typename Ds>
class View<Ptr, T, List<D, Ds>> {
public:
    View(Ptr data) : data(data) {}
    View() : data(new T[ViewSize<List<D, Ds>>::type()]), share(data) {}

    static constexpr int size = D::dim;

    View<Ptr, T, List<D, Ds>>& operator=(const View<Ptr, T, List<D, Ds>>& other) {
        for (int i = 0; i < D::dim; ++i) {
            (*this)[i] = other[i];
        }
        return *this;
    }

    auto operator[](int idx) const {
        return View<Ptr, T, Ds>(data + idx*D::stride);
    }

    //template<typename I, typename... Idx>
    //auto operator()(I i, Idx... idx) const {
    //    return slice<EmptyList, List<D, Ds>>(0, i, idx...);
    //}

    Ptr data;
    std::shared_ptr<T> share;
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

template<typename Ptr, typename T, typename Dims>
std::ostream& operator<<(std::ostream& out, const View<Ptr, T, Dims>& v) {
    out << "{";
    for (int i = 0; i < Dims::head::dim; ++i) {
        out << (i ? "," : "") << v[i];
    }
    return out << "}";
}

template<typename Ptr, typename T>
class View<Ptr, T, EmptyList> {
public:
    View(Ptr data) : data(data) {}
    View() : data(new T), share(data) {}

    auto& operator=(const View<Ptr, T, EmptyList>& other) const {
        *data = *other.data;
        return *this;
    }

    void operator=(T x) const {
        *data = x;
    }

    operator T() const {
        return *data;
    }

    Ptr data;
    std::shared_ptr<T> share;
};

template<typename Ptr, typename T>
std::ostream& operator<<(std::ostream& out, const View<Ptr, T, EmptyList>& v) {
    return out << (T)v;
}
