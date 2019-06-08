#include "evaluator8.h"

namespace {
struct _Cl30 {
    
};
struct _Cl11 {
    
};
struct _Cl7 {
    
};
struct _Cl3 {
    
};
struct _Cl26 {
    
};
struct _Cl23 {
    View<double*, double, to_list_t<P<2,1>>> v227;
};
struct _Cl19 {
    
};
struct _Cl16 {
    double x20;
};

static const std::map<std::string, double*>* userData;
View<double*, double, to_list_t<P<2,6>, P<3,2>, P<2,1>>> _t32;
View<double*, double, to_list_t<P<4,6>, P<3,2>, P<2,1>>> _t29_tmp;

template<typename _T1>
void _lam3(_Cl3 _cl, double x3, double y3, _T1 _result);
double _lam3(_Cl3 _cl, double x3, double y3);
template<typename _T1, typename _T2, typename _T3>
void _lam7(_Cl7 _cl, _T1 v17, _T2 v27, _T3 _result);
template<typename _T1, typename _T2, typename _T3>
void _lam11(_Cl11 _cl, _T1 m111, _T2 m211, _T3 _result);
template<typename _T1>
void _lam16(_Cl16 _cl, double x215, _T1 _result);
double _lam16(_Cl16 _cl, double x215);
template<typename _T1, typename _T2>
void _lam19(_Cl19 _cl, double x20, _T1 v20, _T2 _result);
template<typename _T1>
void _lam23(_Cl23 _cl, double x24, _T1 _result);
template<typename _T1, typename _T2, typename _T3>
void _lam26(_Cl26 _cl, _T1 v127, _T2 v227, _T3 _result);
template<typename _T1, typename _T2>
void _lam30(_Cl30 _cl, _T1 m31, _T2 _result);
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl11 _clRed, _Cl26 _clZip, _T1 _result, _T2 _tmp, _T... vecs);
template<typename _T1, typename... _T>
void _zip0(_Cl3 _clZip, _T1 _result, _T... vecs);
template<typename _T1, typename... _T>
void _zip1(_Cl7 _clZip, _T1 _result, _T... vecs);
template<typename _T1, typename... _T>
void _zip2(_Cl16 _clZip, _T1 _result, _T... vecs);
template<typename _T1, typename... _T>
void _zip3(_Cl23 _clZip, _T1 _result, _T... vecs);
template<typename _T1, typename... _T>
void _zip4(_Cl30 _clZip, _T1 _result, _T... vecs);

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
template<typename _T1, typename _T2, typename _T3>
void _lam11(_Cl11 _cl, _T1 m111, _T2 m211, _T3 _result) {
    _zip1({}, _result, m111, m211);
    
}
template<typename _T1>
void _lam16(_Cl16 _cl, double x215, _T1 _result) {
    auto x115 = _cl.x20;
    _result = (x115) * (x215);
    
}
double _lam16(_Cl16 _cl, double x215) {
    double result;
    _lam16(_cl, x215, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename _T2>
void _lam19(_Cl19 _cl, double x20, _T1 v20, _T2 _result) {
    _zip2({x20}, _result, v20);
    
}
template<typename _T1>
void _lam23(_Cl23 _cl, double x24, _T1 _result) {
    _lam19({}, x24, _cl.v227, _result);
    
}
template<typename _T1, typename _T2, typename _T3>
void _lam26(_Cl26 _cl, _T1 v127, _T2 v227, _T3 _result) {
    _zip3({v227}, _result, v127);
    
}
template<typename _T1, typename _T2>
void _lam30(_Cl30 _cl, _T1 m31, _T2 _result) {
    View<double*, double, to_list_t<P<4,2>, P<2,1>>> _t28(userData->at("mat8"));
    _rnz0({}, {}, _result, _t29_tmp, m31, _t28);
    
}
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl11 _clRed, _Cl26 _clZip, _T1 _result, _T2 _tmp, _T... vecs) {
    for (int i = 0; i < _tmp.size; ++i)
        _lam26(_clZip, vecs[i]..., _tmp[i]);
    _result = _tmp[0];
    for (int i = 1; i < _tmp.size; ++i)
        _lam11(_clRed, _result, _tmp[i], _result);
}
template<typename _T1, typename... _T>
void _zip0(_Cl3 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam3(_clZip, vecs[i]..., _result[i]);
}
template<typename _T1, typename... _T>
void _zip1(_Cl7 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam7(_clZip, vecs[i]..., _result[i]);
}
template<typename _T1, typename... _T>
void _zip2(_Cl16 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam16(_clZip, vecs[i]..., _result[i]);
}
template<typename _T1, typename... _T>
void _zip3(_Cl23 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam23(_clZip, vecs[i]..., _result[i]);
}
template<typename _T1, typename... _T>
void _zip4(_Cl30 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam30(_clZip, vecs[i]..., _result[i]);
}
}

View<double*, double, to_list_t<P<2,6>, P<3,2>, P<2,1>>> evaluator8(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    View<double*, double, to_list_t<P<2,12>, P<4,1>, P<3,4>>> _t31(userData->at("tens"));
    _zip4({}, _t32, _t31);
    return _t32;
}
