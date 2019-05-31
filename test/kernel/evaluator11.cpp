#include "evaluator11.h"

namespace {
struct _Cl4 {
    
};
struct _Cl8 {
    
};

static const std::map<std::string, double*>* userData;
View<double*, double, to_list_t<P<3,1>>> _t11;
View<double*, double, to_list_t<P<3,1>>> _t10;

template<typename _T1>
void _lam4(_Cl4 _cl, double x23, _T1 _result);
double _lam4(_Cl4 _cl, double x23);
template<typename _T1>
void _lam8(_Cl8 _cl, double x9, _T1 _result);
double _lam8(_Cl8 _cl, double x9);
template<typename _T1, typename... _T>
void _zip1(_Cl4 _clZip, _T1 _result, _T... vecs);
template<typename _T1, typename... _T>
void _zip0(_Cl8 _clZip, _T1 _result, _T... vecs);

template<typename _T1>
void _lam4(_Cl4 _cl, double x23, _T1 _result) {
    double x13 = 2.0;
    _result = (x13) * (x23);
    
}
double _lam4(_Cl4 _cl, double x23) {
    double result;
    _lam4(_cl, x23, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1>
void _lam8(_Cl8 _cl, double x9, _T1 _result) {
    _result = (x9) + (1.0);
    
}
double _lam8(_Cl8 _cl, double x9) {
    double result;
    _lam8(_cl, x9, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename... _T>
void _zip1(_Cl4 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam4(_clZip, vecs[i]..., _result[i]);
}
template<typename _T1, typename... _T>
void _zip0(_Cl8 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam8(_clZip, vecs[i]..., _result[i]);
}
}

View<double*, double, to_list_t<P<3,1>>> evaluator11(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    View<double*, double, to_list_t<P<3,1>>> _t9(userData->at("a"));
    _zip0({}, _t10, _t9);
    _zip1({}, _t11, _t10);
    return _t11;
}
