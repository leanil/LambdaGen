#include "evaluator17.h"

namespace {
struct _Cl24 {
    
};
struct _Cl21 {
    View<double*, double, to_list_t<P<3,4>>> u24;
};
struct _Cl3 {
    
};
struct _Cl15 {
    
};
struct _Cl7 {
    
};
struct _Cl11 {
    
};

static const std::map<std::string, double*>* userData;
View<double*, double, to_list_t<P<4,2>, P<2,1>>> _t27;
View<double*, double, to_list_t<P<3,1>>> _t20_tmp;
View<double*, double, to_list_t<P<1,1>>> _t14_tmp;

template<typename _T1>
void _lam3(_Cl3 _cl, double x3, double y3, _T1 _result);
double _lam3(_Cl3 _cl, double x3, double y3);
template<typename _T1>
void _lam7(_Cl7 _cl, double x7, double y7, _T1 _result);
double _lam7(_Cl7 _cl, double x7, double y7);
template<typename _T1>
void _lam11(_Cl11 _cl, double x11, double y11, _T1 _result);
double _lam11(_Cl11 _cl, double x11, double y11);
template<typename _T1, typename _T2, typename _T3>
void _lam15(_Cl15 _cl, _T1 u115, _T2 u215, _T3 _result);
template<typename _T1, typename _T2>
double _lam15(_Cl15 _cl, _T1 u115, _T2 u215);
template<typename _T1, typename _T2>
void _lam21(_Cl21 _cl, _T1 v21, _T2 _result);
template<typename _T1>
double _lam21(_Cl21 _cl, _T1 v21);
template<typename _T1, typename _T2>
void _lam24(_Cl24 _cl, _T1 u24, _T2 _result);
template<typename _T1, typename _T2, typename... _T>
void _rnz1(_Cl3 _clRed, _Cl15 _clZip, _T1 _result, _T2 _tmp, _T... vecs);
template<typename _T1, typename... _T>
double _rnz1w(_Cl3 _clRed, _Cl15 _clZip, _T1 _tmp, _T... vecs);
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl7 _clRed, _Cl11 _clZip, _T1 _result, _T2 _tmp, _T... vecs);
template<typename _T1, typename... _T>
double _rnz0w(_Cl7 _clRed, _Cl11 _clZip, _T1 _tmp, _T... vecs);
template<typename _T1, typename... _T>
void _zip0(_Cl21 _clZip, _T1 _result, _T... vecs);
template<typename _T1, typename... _T>
void _zip1(_Cl24 _clZip, _T1 _result, _T... vecs);

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
    _result = (x7) + (y7);
    
}
double _lam7(_Cl7 _cl, double x7, double y7) {
    double result;
    _lam7(_cl, x7, y7, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1>
void _lam11(_Cl11 _cl, double x11, double y11, _T1 _result) {
    _result = (x11) * (y11);
    
}
double _lam11(_Cl11 _cl, double x11, double y11) {
    double result;
    _lam11(_cl, x11, y11, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename _T2, typename _T3>
void _lam15(_Cl15 _cl, _T1 u115, _T2 u215, _T3 _result) {
    _rnz0({}, {}, _result, _t14_tmp, u115, u215);
    
}
template<typename _T1, typename _T2>
double _lam15(_Cl15 _cl, _T1 u115, _T2 u215) {
    double result;
    _lam15(_cl, u115, u215, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename _T2>
void _lam21(_Cl21 _cl, _T1 v21, _T2 _result) {
    View<double*, double, to_list_t<P<3,4>, P<1,4>>> _t17 = subdiv<0,1>(_cl.u24);
    View<double*, double, to_list_t<P<3,1>, P<1,1>>> _t19 = subdiv<0,1>(v21);
    _rnz1({}, {}, _result, _t20_tmp, _t17, _t19);
    
}
template<typename _T1>
double _lam21(_Cl21 _cl, _T1 v21) {
    double result;
    _lam21(_cl, v21, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename _T2>
void _lam24(_Cl24 _cl, _T1 u24, _T2 _result) {
    View<double*, double, to_list_t<P<2,3>, P<3,1>>> _t22(userData->at("M1"));
    _zip0({u24}, _result, _t22);
    
}
template<typename _T1, typename _T2, typename... _T>
void _rnz1(_Cl3 _clRed, _Cl15 _clZip, _T1 _result, _T2 _tmp, _T... vecs) {
    for (int i = 0; i < _tmp.size; ++i)
        _lam15(_clZip, vecs[i]..., _tmp[i]);
    _result = _tmp[0];
    for (int i = 1; i < _tmp.size; ++i)
        _lam3(_clRed, _result, _tmp[i], _result);
}
template<typename _T1, typename... _T>
double _rnz1w(_Cl3 _clRed, _Cl15 _clZip, _T1 _tmp, _T... vecs) {
    double result;
    _rnz1(_clRed, _clZip, View<double*, double, to_list_t<>, true>(&result), _tmp, vecs...);
    return result;
}
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl7 _clRed, _Cl11 _clZip, _T1 _result, _T2 _tmp, _T... vecs) {
    for (int i = 0; i < _tmp.size; ++i)
        _lam11(_clZip, vecs[i]..., _tmp[i]);
    _result = _tmp[0];
    for (int i = 1; i < _tmp.size; ++i)
        _lam7(_clRed, _result, _tmp[i], _result);
}
template<typename _T1, typename... _T>
double _rnz0w(_Cl7 _clRed, _Cl11 _clZip, _T1 _tmp, _T... vecs) {
    double result;
    _rnz0(_clRed, _clZip, View<double*, double, to_list_t<>, true>(&result), _tmp, vecs...);
    return result;
}
template<typename _T1, typename... _T>
void _zip0(_Cl21 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam21(_clZip, vecs[i]..., _result[i]);
}
template<typename _T1, typename... _T>
void _zip1(_Cl24 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam24(_clZip, vecs[i]..., _result[i]);
}
}

View<double*, double, to_list_t<P<4,2>, P<2,1>>> evaluator17(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    View<double*, double, to_list_t<P<3,4>, P<4,1>>> _t25(userData->at("M2"));
    View<double*, double, to_list_t<P<4,1>, P<3,4>>> _t26 = flip<0>(_t25);
    _zip1({}, _t27, _t26);
    return _t27;
}
