#pragma once

#include <type_traits>

template<typename Head, typename Tail>
struct List {
    using head = Head;
    using tail = Tail;
};

struct EmptyList;

template<typename... Xs> struct to_list;

template<typename X, typename... Xs>
struct to_list<X, Xs...> {
    using type = List<X, typename to_list<Xs...>::type>;
};

template<>
struct to_list<> {
    using type = EmptyList;
};

template<typename... Xs>
using to_list_t = typename to_list<Xs...>::type;

template<typename L1, typename L2>
struct Concat {
    using type = List<typename L1::head, typename Concat<typename L1::tail, L2>::type>;
};

template<typename L2>
struct Concat<EmptyList, L2> {
    using type = L2;
};

template<typename L, typename X>
struct Insert {
    using type = std::conditional_t<(typename X::key() < typename L::head::key()), List<X, L>, List<typename L::head, typename Insert<typename L::tail, X>::type>>;
};

template<typename X>
struct Insert<EmptyList, X> {
    using type = List<X, EmptyList>;
};

template<typename L, template<class> class F>
struct Map {
    using type = List<typename F<typename L::head>::type, typename Map<typename L::tail, F>::type>;
};

template<template<class> class F>
struct Map<EmptyList, F> {
    using type = EmptyList;
};

template<template<class,class> class F, typename A, typename L>
struct Foldr {
    using type = typename F<typename L::head, typename Foldr<F, A, typename L::tail>::type>::type;
};

template<template<class, class> class F, typename A>
struct Foldr<F, A, EmptyList> {
    using type = A;
};

template<int x>
struct Int {
    constexpr operator int() { return x; }
};

template<typename K, typename V>
struct KeyValuePair {
    using key = K;
    using value = V;
};

template<typename> struct get_value;

template<typename K, typename V>
struct get_value<KeyValuePair<K, V>> {
    using type = V;
};