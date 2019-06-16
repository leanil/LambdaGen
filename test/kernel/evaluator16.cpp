#include "evaluator16.h"

namespace {
struct _Cl26 {
    
};
struct _Cl23 {
    View<double*, double, to_list_t<P<3,4>>> u26;
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
View<double*, double, to_list_t<P<4,2>, P<2,1>>> _t29;
View<double*, double, to_list_t<>> _t22_tmp;
View<double*, double, to_list_t<>> _t14_tmp;

template<typename _T1>
void _lam3(_Cl3 _cl, double x3, double y3, _T1 _result);
double _lam3(_Cl3 _cl, double x3, double y3);
template<typename _T1>
void _lam7(_Cl7 _cl, double x7, double y7, _T1 _result);
double _lam7(_Cl7 _cl, double x7, double y7);
template<typename _T1>
void _lam11(_Cl11 _cl, double x111, double x211, _T1 _result);
double _lam11(_Cl11 _cl, double x111, double x211);
template<typename _T1, typename _T2, typename _T3>
void _lam15(_Cl15 _cl, _T1 u115, _T2 u215, _T3 _result);
template<typename _T1, typename _T2>
double _lam15(_Cl15 _cl, _T1 u115, _T2 u215);
template<typename _T1, typename _T2>
void _lam23(_Cl23 _cl, _T1 v23, _T2 _result);
template<typename _T1>
double _lam23(_Cl23 _cl, _T1 v23);
template<typename _T1, typename _T2>
void _lam26(_Cl26 _cl, _T1 u26, _T2 _result);
template<typename _T1, typename _T2, typename... _T>
void _rnz1(_Cl3 _clRed, _Cl15 _clZip, _T1 _result, _T2 _tmp, _T... vecs);
template<typename _T1, typename... _T>
double _rnz1w(_Cl3 _clRed, _Cl15 _clZip, _T1 _tmp, _T... vecs);
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl7 _clRed, _Cl11 _clZip, _T1 _result, _T2 _tmp, _T... vecs);
template<typename _T1, typename... _T>
double _rnz0w(_Cl7 _clRed, _Cl11 _clZip, _T1 _tmp, _T... vecs);
template<typename _T1, typename... _T>
void _zip0(_Cl23 _clZip, _T1 _result, _T... vecs);
template<typename _T1, typename... _T>
void _zip1(_Cl26 _clZip, _T1 _result, _T... vecs);

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
void _lam11(_Cl11 _cl, double x111, double x211, _T1 _result) {
    _result = (x111) * (x211);
    
}
double _lam11(_Cl11 _cl, double x111, double x211) {
    double result;
    _lam11(_cl, x111, x211, View<double*, double, to_list_t<>, true>(&result));
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
void _lam23(_Cl23 _cl, _T1 v23, _T2 _result) {
    View<double*, double, to_list_t<P<3,4>, P<1,4>>> _t17 = subdiv<0,1>(_cl.u26);
    View<double*, double, to_list_t<P<1,4>, P<3,4>>> _t18 = flip<0>(_t17);
    View<double*, double, to_list_t<P<3,1>, P<1,1>>> _t20 = subdiv<0,1>(v23);
    View<double*, double, to_list_t<P<1,1>, P<3,1>>> _t21 = flip<0>(_t20);
    _rnz1({}, {}, _result, _t22_tmp, _t18, _t21);
    
}
template<typename _T1>
double _lam23(_Cl23 _cl, _T1 v23) {
    double result;
    _lam23(_cl, v23, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename _T2>
void _lam26(_Cl26 _cl, _T1 u26, _T2 _result) {
    View<double*, double, to_list_t<P<2,3>, P<3,1>>> _t24(userData->at("M1"));
    _zip0({u26}, _result, _t24);
    
}
template<typename _T1, typename _T2, typename... _T>
void _rnz1(_Cl3 _clRed, _Cl15 _clZip, _T1 _result, _T2 _tmp, _T... vecs) {
    _lam15(_clZip, vecs[0]..., _result);
    for (int i = 1; i < head<_T...>::size; ++i) {
        _lam15(_clZip, vecs[i]..., _tmp);
        _lam3(_clRed, _result, _tmp, _result);
    }
}
template<typename _T1, typename... _T>
double _rnz1w(_Cl3 _clRed, _Cl15 _clZip, _T1 _tmp, _T... vecs) {
    double result;
    _rnz1(_clRed, _clZip, View<double*, double, to_list_t<>, true>(&result), _tmp, vecs...);
    return result;
}
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl7 _clRed, _Cl11 _clZip, _T1 _result, _T2 _tmp, _T... vecs) {
    _lam11(_clZip, vecs[0]..., _result);
    for (int i = 1; i < head<_T...>::size; ++i) {
        _lam11(_clZip, vecs[i]..., _tmp);
        _lam7(_clRed, _result, _tmp, _result);
    }
}
template<typename _T1, typename... _T>
double _rnz0w(_Cl7 _clRed, _Cl11 _clZip, _T1 _tmp, _T... vecs) {
    double result;
    _rnz0(_clRed, _clZip, View<double*, double, to_list_t<>, true>(&result), _tmp, vecs...);
    return result;
}
template<typename _T1, typename... _T>
void _zip0(_Cl23 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam23(_clZip, vecs[i]..., _result[i]);
}
template<typename _T1, typename... _T>
void _zip1(_Cl26 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam26(_clZip, vecs[i]..., _result[i]);
}
}

View<double*, double, to_list_t<P<4,2>, P<2,1>>> evaluator16(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    View<double*, double, to_list_t<P<3,4>, P<4,1>>> _t27(userData->at("M2"));
    View<double*, double, to_list_t<P<4,1>, P<3,4>>> _t28 = flip<0>(_t27);
    _zip1({}, _t29, _t28);
    return _t29;
}
