#pragma once

#include <iostream>
#include <numeric>
#include <utility>
#include <vector>

using namespace std;

template<size_t D, size_t S>
struct Pair {
	static constexpr size_t dim = D;
	static constexpr size_t stride = S;
};

template<typename D1, typename... Dims>
struct Size {
	static constexpr size_t result = D1::dim * Size<Dims...>::result;
};

template<typename D>
struct Size<D> {
	static constexpr size_t result = D::dim;
};

template<typename T, typename... Dims> class View {};

template<typename T, typename D1, typename... Dims>
class View<T, D1, Dims...> {
public:
	View() : loc{ new T[Size<D1, Dims...>::result] } {}

	View(T* loc) : loc{ loc } {}

	void operator=(const View<T, D1, Dims...>& x) {
		loc = x.loc;
	}

	void copy(const View<T, D1, Dims...>& x) const {
		for (size_t i = 0; i < D1::dim; ++i) {
			(*this)[i].copy(x[i]);
		}
	}

	auto operator[](size_t idx) const {
		return View<T, Dims...>(loc + idx*D1::stride);
	}
private:
	T* loc;
};

template<typename T, typename D1, typename... Dims>
ostream& operator<<(ostream& out, const View<T, D1, Dims...>& v) {
	out << "{";
	for (size_t i = 0; i < D1::dim; ++i) {
		out << (i ? "," : "") << v[i];
	}
	return out << "}";
}

template<typename T>
class View<T> {
public:
	View() : loc{ new T } {}

	View(T* loc) : loc{ loc } {}

	//View(double d) : View(new double{ d }) {}

	View<T>& operator=(const View<T>& x) const {
		*loc = *x.loc;
		return *this;
	}

	void copy(T t) const {
		*loc = t;
	}

	T& operator=(const T& x) const { return *loc = x; }

	operator T() const { return *loc; }
private:
	T* loc;
};

template<typename T>
ostream& operator<<(ostream& out, const View<T>& v) {
	return out << (T)v;
}

//template<typename T, typename D, typename... Dims>
//using View<View<T, Dims...>, D> = View<T, D, Dims...>;

template<typename T, size_t D1>
struct ViewComposer {
	using type = View<T, Pair<D1, 1>>;
};

template<typename T, size_t D1, typename... Dims>
struct ViewComposer<View<T, Dims...>, D1> {
	using type = View<T, Pair<D1, Size<Dims...>::result>, Dims...>;
};

