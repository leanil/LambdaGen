#include "evaluator22.h"

namespace {
struct _Cl3 {
    
};
struct _Cl7 {
    
};

static const std::map<std::string, double*>* userData;
View<double*, double, to_list_t<>> _t10_tmp;

template<typename _T1>
void _lam3(_Cl3 _cl, double x3, double y3, _T1 _result);
double _lam3(_Cl3 _cl, double x3, double y3);
template<typename _T1>
void _lam7(_Cl7 _cl, double x17, double x27, _T1 _result);
double _lam7(_Cl7 _cl, double x17, double x27);
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl3 _clRed, _Cl7 _clZip, _T1 _result, _T2 _tmp, _T... vecs);
template<typename _T1, typename... _T>
double _rnz0w(_Cl3 _clRed, _Cl7 _clZip, _T1 _tmp, _T... vecs);

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
}

double evaluator22(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    View<double*, double, to_list_t<P<3,1>>> _t8(userData->at("a"));
    View<double*, double, to_list_t<P<3,1>>> _t9(userData->at("b"));
    return _rnz0w({}, {}, _t10_tmp, _t8, _t9);
}
