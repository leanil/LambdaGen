#include "evaluator14.h"

namespace {
struct _Cl19 {
    
};
struct _Cl7 {
    
};
struct _Cl3 {
    
};
struct _Cl14 {
    
};
struct _Cl11 {
    double x14;
};

static const std::map<std::string, double*>* userData;
View<double*, double, to_list_t<P<4,2>, P<2,1>>> _t22;
View<double*, double, to_list_t<P<3,2>, P<2,1>>> _t18_tmp;

template<typename _T1>
void _lam3(_Cl3 _cl, double x3, double y3, _T1 _result);
double _lam3(_Cl3 _cl, double x3, double y3);
template<typename _T1, typename _T2, typename _T3>
void _lam7(_Cl7 _cl, _T1 u17, _T2 u27, _T3 _result);
template<typename _T1>
void _lam11(_Cl11 _cl, double y11, _T1 _result);
double _lam11(_Cl11 _cl, double y11);
template<typename _T1, typename _T2>
void _lam14(_Cl14 _cl, double x14, _T1 v14, _T2 _result);
template<typename _T1, typename _T2>
void _lam19(_Cl19 _cl, _T1 u19, _T2 _result);
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl7 _clRed, _Cl14 _clZip, _T1 _result, _T2 _tmp, _T... vecs);
template<typename _T1, typename... _T>
void _zip0(_Cl3 _clZip, _T1 _result, _T... vecs);
template<typename _T1, typename... _T>
void _zip1(_Cl11 _clZip, _T1 _result, _T... vecs);
template<typename _T1, typename... _T>
void _zip2(_Cl19 _clZip, _T1 _result, _T... vecs);

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
void _lam7(_Cl7 _cl, _T1 u17, _T2 u27, _T3 _result) {
    _zip0({}, _result, u17, u27);
    
}
template<typename _T1>
void _lam11(_Cl11 _cl, double y11, _T1 _result) {
    _result = (_cl.x14) * (y11);
    
}
double _lam11(_Cl11 _cl, double y11) {
    double result;
    _lam11(_cl, y11, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename _T2>
void _lam14(_Cl14 _cl, double x14, _T1 v14, _T2 _result) {
    _zip1({x14}, _result, v14);
    
}
template<typename _T1, typename _T2>
void _lam19(_Cl19 _cl, _T1 u19, _T2 _result) {
    View<double*, double, to_list_t<P<2,3>, P<3,1>>> _t16(userData->at("M1"));
    View<double*, double, to_list_t<P<3,1>, P<2,3>>> _t17 = flip<0>(_t16);
    _rnz0({}, {}, _result, _t18_tmp, u19, _t17);
    
}
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl7 _clRed, _Cl14 _clZip, _T1 _result, _T2 _tmp, _T... vecs) {
    for (int i = 0; i < _tmp.size; ++i)
        _lam14(_clZip, vecs[i]..., _tmp[i]);
    _result = _tmp[0];
    for (int i = 1; i < _tmp.size; ++i)
        _lam7(_clRed, _result, _tmp[i], _result);
}
template<typename _T1, typename... _T>
void _zip0(_Cl3 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam3(_clZip, vecs[i]..., _result[i]);
}
template<typename _T1, typename... _T>
void _zip1(_Cl11 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam11(_clZip, vecs[i]..., _result[i]);
}
template<typename _T1, typename... _T>
void _zip2(_Cl19 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam19(_clZip, vecs[i]..., _result[i]);
}
}

View<double*, double, to_list_t<P<4,2>, P<2,1>>> evaluator14(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    View<double*, double, to_list_t<P<3,4>, P<4,1>>> _t20(userData->at("M2"));
    View<double*, double, to_list_t<P<4,1>, P<3,4>>> _t21 = flip<0>(_t20);
    _zip2({}, _t22, _t21);
    return _t22;
}
