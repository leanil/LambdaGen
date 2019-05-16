#include "evaluator7.h"

namespace {
struct _Cl18 {
    
};
struct _Cl15 {
    View<double*, double, to_list_t<P<2,1>, P<3,2>>> m219;
};
struct _Cl12 {
    View<double*, double, to_list_t<P<3,1>>> v16;
};
struct _Cl4 {
    
};
struct _Cl8 {
    
};

static const std::map<std::string, double*>* userData;
View<double*, double, to_list_t<P<2,2>, P<2,1>>> _t21;
View<double*, double, to_list_t<P<3,1>>> _t11_tmp;

template<typename _T1>
void _lam4(_Cl4 _cl, double x3, double y3, _T1 _result);
double _lam4(_Cl4 _cl, double x3, double y3);
template<typename _T1>
void _lam8(_Cl8 _cl, double x7, double y7, _T1 _result);
double _lam8(_Cl8 _cl, double x7, double y7);
template<typename _T1, typename _T2>
void _lam12(_Cl12 _cl, _T1 v211, _T2 _result);
template<typename _T1>
double _lam12(_Cl12 _cl, _T1 v211);
template<typename _T1, typename _T2>
void _lam15(_Cl15 _cl, _T1 v16, _T2 _result);
template<typename _T1, typename _T2, typename _T3>
void _lam18(_Cl18 _cl, _T1 m119, _T2 m219, _T3 _result);
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl4 _clRed, _Cl8 _clZip, _T1 _result, _T2 _tmp, _T... vecs);
template<typename _T1, typename... _T>
double _rnz0w(_Cl4 _clRed, _Cl8 _clZip, _T1 _tmp, _T... vecs);
template<typename _T1, typename... _T>
void _zip0(_Cl12 _clZip, _T1 _result, _T... vecs);
template<typename _T1, typename... _T>
void _zip1(_Cl15 _clZip, _T1 _result, _T... vecs);

template<typename _T1>
void _lam4(_Cl4 _cl, double x3, double y3, _T1 _result) {
    _result = (x3) + (y3);
    
}
double _lam4(_Cl4 _cl, double x3, double y3) {
    double result;
    _lam4(_cl, x3, y3, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1>
void _lam8(_Cl8 _cl, double x7, double y7, _T1 _result) {
    _result = (x7) * (y7);
    
}
double _lam8(_Cl8 _cl, double x7, double y7) {
    double result;
    _lam8(_cl, x7, y7, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename _T2>
void _lam12(_Cl12 _cl, _T1 v211, _T2 _result) {
    auto v111 = _cl.v16;
    _rnz0({}, {}, _result, _t11_tmp, v111, v211);
    
}
template<typename _T1>
double _lam12(_Cl12 _cl, _T1 v211) {
    double result;
    _lam12(_cl, v211, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename _T2>
void _lam15(_Cl15 _cl, _T1 v16, _T2 _result) {
    _zip0({v16}, _result, _cl.m219);
    
}
template<typename _T1, typename _T2, typename _T3>
void _lam18(_Cl18 _cl, _T1 m119, _T2 m219, _T3 _result) {
    _zip1({m219}, _result, m119);
    
}
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl4 _clRed, _Cl8 _clZip, _T1 _result, _T2 _tmp, _T... vecs) {
    for (int i = 0; i < _tmp.size; ++i)
        _lam8(_clZip, vecs[i]..., _tmp[i]);
    _result = _tmp[0];
    for (int i = 1; i < _tmp.size; ++i)
        _lam4(_clRed, _result, _tmp[i], _result);
}
template<typename _T1, typename... _T>
double _rnz0w(_Cl4 _clRed, _Cl8 _clZip, _T1 _tmp, _T... vecs) {
    double result;
    _rnz0(_clRed, _clZip, View<double*, double, to_list_t<>, true>(&result), _tmp, vecs...);
    return result;
}
template<typename _T1, typename... _T>
void _zip0(_Cl12 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam12(_clZip, vecs[i]..., _result[i]);
}
template<typename _T1, typename... _T>
void _zip1(_Cl15 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam15(_clZip, vecs[i]..., _result[i]);
}
}

View<double*, double, to_list_t<P<2,2>, P<2,1>>> evaluator7(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    View<double*, double, to_list_t<P<2,3>, P<3,1>>> _t19(userData->at("mat"));
    View<double*, double, to_list_t<P<2,1>, P<3,2>>> _t20(userData->at("mat"));
    _lam18({}, _t19, _t20, _t21);
    return _t21;
}
