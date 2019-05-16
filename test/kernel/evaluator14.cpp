#include "evaluator14.h"

namespace {
struct _Cl13 {
    
};
struct _Cl7 {
    
};
struct _Cl4 {
    double x8;
};

static const std::map<std::string, double*>* userData;
View<double*, double, to_list_t<P<3,3>, P<3,1>>> _t15;
View<double*, double, to_list_t<P<3,1>>> _t11;

template<typename _T1>
void _lam4(_Cl4 _cl, double y3, _T1 _result);
double _lam4(_Cl4 _cl, double y3);
template<typename _T1>
void _lam7(_Cl7 _cl, double x8, _T1 _result);
template<typename _T1>
void _lam13(_Cl13 _cl, double y14, _T1 _result);
template<typename _T1, typename... _T>
void _zip0(_Cl4 _clZip, _T1 _result, _T... vecs);
template<typename _T1, typename... _T>
void _zip1(_Cl7 _clZip, _T1 _result, _T... vecs);

template<typename _T1>
void _lam4(_Cl4 _cl, double y3, _T1 _result) {
    auto x3 = _cl.x8;
    _result = (x3) + (y3);
    
}
double _lam4(_Cl4 _cl, double y3) {
    double result;
    _lam4(_cl, y3, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1>
void _lam7(_Cl7 _cl, double x8, _T1 _result) {
    View<double*, double, to_list_t<P<3,1>>> _t5(userData->at("a"));
    _zip0({x8}, _result, _t5);
    
}
template<typename _T1>
void _lam13(_Cl13 _cl, double y14, _T1 _result) {
    _Cl7 f14 = {};
    _lam7(f14, y14, _t11);
    _zip1(f14, _result, _t11);
    
}
template<typename _T1, typename... _T>
void _zip0(_Cl4 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam4(_clZip, vecs[i]..., _result[i]);
}
template<typename _T1, typename... _T>
void _zip1(_Cl7 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam7(_clZip, vecs[i]..., _result[i]);
}
}

View<double*, double, to_list_t<P<3,3>, P<3,1>>> evaluator14(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    _lam13({}, 1.0, _t15);
    return _t15;
}
