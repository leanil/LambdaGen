#include "evaluator9.h"

namespace {
struct _Cl3 {
    
};
struct _Cl7 {
    
};

static const std::map<std::string, double*>* userData;
View<double*, double, to_list_t<P<3,1>>> _t11;
View<double*, double, to_list_t<P<3,1>>> _t9;

template<typename _T1>
void _lam3(_Cl3 _cl, double x3, double y3, _T1 _result);
double _lam3(_Cl3 _cl, double x3, double y3);
template<typename _T1>
void _lam7(_Cl7 _cl, double x7, _T1 _result);
double _lam7(_Cl7 _cl, double x7);
template<typename _T1, typename... _T>
void _zip1(_Cl3 _clZip, _T1 _result, _T... vecs);
template<typename _T1, typename... _T>
void _zip0(_Cl7 _clZip, _T1 _result, _T... vecs);

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
void _lam7(_Cl7 _cl, double x7, _T1 _result) {
    _result = (3.0) * (x7);
    
}
double _lam7(_Cl7 _cl, double x7) {
    double result;
    _lam7(_cl, x7, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename... _T>
void _zip1(_Cl3 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam3(_clZip, vecs[i]..., _result[i]);
}
template<typename _T1, typename... _T>
void _zip0(_Cl7 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam7(_clZip, vecs[i]..., _result[i]);
}
}

View<double*, double, to_list_t<P<3,1>>> evaluator9(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    View<double*, double, to_list_t<P<3,1>>> _t8(userData->at("a"));
    _zip0({}, _t9, _t8);
    View<double*, double, to_list_t<P<3,1>>> _t10(userData->at("b"));
    _zip1({}, _t11, _t9, _t10);
    return _t11;
}
