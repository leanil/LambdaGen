#include "evaluator15.h"

namespace {
struct _Cl16 {
    
};
struct _Cl11 {
    View<double*, double, to_list_t<P<3,4>>> u16;
};
struct _Cl3 {
    
};
struct _Cl7 {
    
};

static const std::map<std::string, double*>* userData;
View<double*, double, to_list_t<P<4,2>, P<2,1>>> _t19;
View<double*, double, to_list_t<>> _t10_tmp;

template<typename _T1>
void _lam3(_Cl3 _cl, double x3, double y3, _T1 _result);
double _lam3(_Cl3 _cl, double x3, double y3);
template<typename _T1>
void _lam7(_Cl7 _cl, double x17, double x27, _T1 _result);
double _lam7(_Cl7 _cl, double x17, double x27);
template<typename _T1, typename _T2>
void _lam11(_Cl11 _cl, _T1 v11, _T2 _result);
template<typename _T1>
double _lam11(_Cl11 _cl, _T1 v11);
template<typename _T1, typename _T2>
void _lam16(_Cl16 _cl, _T1 u16, _T2 _result);
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl3 _clRed, _Cl7 _clZip, _T1 _result, _T2 _tmp, _T... vecs);
template<typename _T1, typename... _T>
double _rnz0w(_Cl3 _clRed, _Cl7 _clZip, _T1 _tmp, _T... vecs);
template<typename _T1, typename... _T>
void _zip0(_Cl11 _clZip, _T1 _result, _T... vecs);
template<typename _T1, typename... _T>
void _zip1(_Cl16 _clZip, _T1 _result, _T... vecs);

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
void _lam7(_Cl7 _cl, double x17, double x27, _T1 _result) {
    _result = (x17) * (x27);
    
}
double _lam7(_Cl7 _cl, double x17, double x27) {
    double result;
    _lam7(_cl, x17, x27, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename _T2>
void _lam11(_Cl11 _cl, _T1 v11, _T2 _result) {
    _rnz0({}, {}, _result, _t10_tmp, _cl.u16, v11);
    
}
template<typename _T1>
double _lam11(_Cl11 _cl, _T1 v11) {
    double result;
    _lam11(_cl, v11, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename _T2>
void _lam16(_Cl16 _cl, _T1 u16, _T2 _result) {
    View<double*, double, to_list_t<P<2,3>, P<3,1>>> _t12(userData->at("M1"));
    View<double*, double, to_list_t<P<3,1>, P<2,3>>> _t13 = flip<0>(_t12);
    View<double*, double, to_list_t<P<2,3>, P<3,1>>> _t14 = flip<0>(_t13);
    _zip0({u16}, _result, _t14);
    
}
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl3 _clRed, _Cl7 _clZip, _T1 _result, _T2 _tmp, _T... vecs) {
    double tmp, result;
    result = _lam7(_clZip, vecs[0]...);
    for (int i = 1; i < head<_T...>::size; ++i) {
        tmp = _lam7(_clZip, vecs[i]...);
        result = _lam3(_clRed, result, tmp);
    }
    _result = result;
}
template<typename _T1, typename... _T>
double _rnz0w(_Cl3 _clRed, _Cl7 _clZip, _T1 _tmp, _T... vecs) {
    double result;
    _rnz0(_clRed, _clZip, View<double*, double, to_list_t<>, true>(&result), _tmp, vecs...);
    return result;
}
template<typename _T1, typename... _T>
void _zip0(_Cl11 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam11(_clZip, vecs[i]..., _result[i]);
}
template<typename _T1, typename... _T>
void _zip1(_Cl16 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam16(_clZip, vecs[i]..., _result[i]);
}
}

View<double*, double, to_list_t<P<4,2>, P<2,1>>> evaluator15(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    View<double*, double, to_list_t<P<3,4>, P<4,1>>> _t17(userData->at("M2"));
    View<double*, double, to_list_t<P<4,1>, P<3,4>>> _t18 = flip<0>(_t17);
    _zip1({}, _t19, _t18);
    return _t19;
}
