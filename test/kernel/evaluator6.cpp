#include "evaluator6.h"

namespace {
struct _Cl14 {
    
};
struct _Cl11 {
    View<double*, double, to_list_t<P<3,1>>> v215;
};
struct _Cl7 {
    
};
struct _Cl4 {
    double x8;
};

static const std::map<std::string, double*>* userData;
View<double*, double, to_list_t<P<3,3>, P<3,1>>> _t17;

template<typename _T1>
void _lam4(_Cl4 _cl, double x23, _T1 _result);
double _lam4(_Cl4 _cl, double x23);
template<typename _T1, typename _T2>
void _lam7(_Cl7 _cl, double x8, _T1 v8, _T2 _result);
template<typename _T1>
void _lam11(_Cl11 _cl, double x12, _T1 _result);
template<typename _T1, typename _T2, typename _T3>
void _lam14(_Cl14 _cl, _T1 v115, _T2 v215, _T3 _result);
template<typename _T1, typename... _T>
void _zip0(_Cl4 _clZip, _T1 _result, _T... vecs);
template<typename _T1, typename... _T>
void _zip1(_Cl11 _clZip, _T1 _result, _T... vecs);

template<typename _T1>
void _lam4(_Cl4 _cl, double x23, _T1 _result) {
    auto x13 = _cl.x8;
    _result = (x13) * (x23);
    
}
double _lam4(_Cl4 _cl, double x23) {
    double result;
    _lam4(_cl, x23, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename _T2>
void _lam7(_Cl7 _cl, double x8, _T1 v8, _T2 _result) {
    _zip0({x8}, _result, v8);
    
}
template<typename _T1>
void _lam11(_Cl11 _cl, double x12, _T1 _result) {
    _lam7({}, x12, _cl.v215, _result);
    
}
template<typename _T1, typename _T2, typename _T3>
void _lam14(_Cl14 _cl, _T1 v115, _T2 v215, _T3 _result) {
    _zip1({v215}, _result, v115);
    
}
template<typename _T1, typename... _T>
void _zip0(_Cl4 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam4(_clZip, vecs[i]..., _result[i]);
}
template<typename _T1, typename... _T>
void _zip1(_Cl11 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam11(_clZip, vecs[i]..., _result[i]);
}
}

View<double*, double, to_list_t<P<3,3>, P<3,1>>> evaluator6(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    View<double*, double, to_list_t<P<3,1>>> _t15(userData->at("a"));
    View<double*, double, to_list_t<P<3,1>>> _t16(userData->at("b"));
    _lam14({}, _t15, _t16, _t17);
    return _t17;
}
