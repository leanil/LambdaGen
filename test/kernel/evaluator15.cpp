#include "evaluator15.h"

namespace {
struct _Cl3 {
    
};
struct _Cl7 {
    
};

static const std::map<std::string, double*>* userData;
View<double*, double, to_list_t<P<3,1>>> _t10_tmp;

template<typename _T1>
void _lam3(_Cl3 _cl, double x3, double y3, _T1 _result);
double _lam3(_Cl3 _cl, double x3, double y3);
template<typename _T1>
void _lam7(_Cl7 _cl, double x7, double y7, _T1 _result);
double _lam7(_Cl7 _cl, double x7, double y7);
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl3 _clRed, _Cl7 _clZip, _T1 _result, _T2 _tmp, _T... vecs);
template<typename _T1, typename... _T>
double _rnz0w(_Cl3 _clRed, _Cl7 _clZip, _T1 _tmp, _T... vecs);

template<typename _T1>
void _lam3(_Cl3 _cl, double x3, double y3, _T1 _result) {
    _result = (x3) + (y3);
    
}
double _lam3(_Cl3 _cl, double x3, double y3) {
    double result;
    _lam3(_cl, x3, y3, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1>
void _lam7(_Cl7 _cl, double x7, double y7, _T1 _result) {
    _result = (x7) * (y7);
    
}
double _lam7(_Cl7 _cl, double x7, double y7) {
    double result;
    _lam7(_cl, x7, y7, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl3 _clRed, _Cl7 _clZip, _T1 _result, _T2 _tmp, _T... vecs) {
    for (int i = 0; i < _tmp.size; ++i)
        _lam7(_clZip, vecs[i]..., _tmp[i]);
    _result = _tmp[0];
    for (int i = 1; i < _tmp.size; ++i)
        _lam3(_clRed, _result, _tmp[i], _result);
}
template<typename _T1, typename... _T>
double _rnz0w(_Cl3 _clRed, _Cl7 _clZip, _T1 _tmp, _T... vecs) {
    double result;
    _rnz0(_clRed, _clZip, View<double*, double, to_list_t<>, true>(&result), _tmp, vecs...);
    return result;
}
}

double evaluator15(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    View<double*, double, to_list_t<P<3,1>>> _t8(userData->at("a"));
    View<double*, double, to_list_t<P<3,1>>> _t9(userData->at("b"));
    return _rnz0w({}, {}, _t10_tmp, _t8, _t9);
}
