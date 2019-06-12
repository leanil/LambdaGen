#include "evaluator5.h"

namespace {
struct _Cl7 {
    
};
struct _Cl3 {
    
};
struct _Cl15 {
    
};
struct _Cl12 {
    double x16;
};

static const std::map<std::string, double*>* userData;
View<double*, double, to_list_t<P<2,1>>> _t18;
View<double*, double, to_list_t<P<2,1>>> _t18_tmp;

template<typename _T1>
void _lam3(_Cl3 _cl, double x3, double y3, _T1 _result);
double _lam3(_Cl3 _cl, double x3, double y3);
template<typename _T1, typename _T2, typename _T3>
void _lam7(_Cl7 _cl, _T1 v17, _T2 v27, _T3 _result);
template<typename _T1>
void _lam12(_Cl12 _cl, double x211, _T1 _result);
double _lam12(_Cl12 _cl, double x211);
template<typename _T1, typename _T2>
void _lam15(_Cl15 _cl, double x16, _T1 v16, _T2 _result);
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl7 _clRed, _Cl15 _clZip, _T1 _result, _T2 _tmp, _T... vecs);
template<typename _T1, typename... _T>
void _zip0(_Cl3 _clZip, _T1 _result, _T... vecs);
template<typename _T1, typename... _T>
void _zip1(_Cl12 _clZip, _T1 _result, _T... vecs);

template<typename _T1>
void _lam3(_Cl3 _cl, double x3, double y3, _T1 _result) {
    _result = (x3) + (y3);
    
}
double _lam3(_Cl3 _cl, double x3, double y3) {
    double result;
    _lam3(_cl, x3, y3, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename _T2, typename _T3>
void _lam7(_Cl7 _cl, _T1 v17, _T2 v27, _T3 _result) {
    _zip0({}, _result, v17, v27);
    
}
template<typename _T1>
void _lam12(_Cl12 _cl, double x211, _T1 _result) {
    auto x111 = _cl.x16;
    _result = (x111) * (x211);
    
}
double _lam12(_Cl12 _cl, double x211) {
    double result;
    _lam12(_cl, x211, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename _T2>
void _lam15(_Cl15 _cl, double x16, _T1 v16, _T2 _result) {
    _zip1({x16}, _result, v16);
    
}
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl7 _clRed, _Cl15 _clZip, _T1 _result, _T2 _tmp, _T... vecs) {
    _lam15(_clZip, vecs[0]..., _result);
    for (int i = 1; i < size<_T...>(); ++i) {
        _lam15(_clZip, vecs[i]..., _tmp);
        _lam7(_clRed, _result, _tmp, _result);
    }
}
template<typename _T1, typename... _T>
void _zip0(_Cl3 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam3(_clZip, vecs[i]..., _result[i]);
}
template<typename _T1, typename... _T>
void _zip1(_Cl12 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam12(_clZip, vecs[i]..., _result[i]);
}
}

View<double*, double, to_list_t<P<2,1>>> evaluator5(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    View<double*, double, to_list_t<P<3,1>>> _t16(userData->at("vec"));
    View<double*, double, to_list_t<P<3,1>, P<2,3>>> _t17(userData->at("mat"));
    _rnz0({}, {}, _t18, _t18_tmp, _t16, _t17);
    return _t18;
}
