#include "evaluator4.h"

namespace {
struct _Cl12 {
    
};
struct _Cl4 {
    
};
struct _Cl8 {
    
};

static const std::map<std::string, double*>* userData;
View<double*, double, to_list_t<P<2,1>>> _t14;
View<double*, double, to_list_t<>> _t11_tmp;

template<typename _T1>
void _lam4(_Cl4 _cl, double x3, double y3, _T1 _result);
double _lam4(_Cl4 _cl, double x3, double y3);
template<typename _T1>
void _lam8(_Cl8 _cl, double x17, double x27, _T1 _result);
double _lam8(_Cl8 _cl, double x17, double x27);
template<typename _T1, typename _T2>
void _lam12(_Cl12 _cl, _T1 v211, _T2 _result);
template<typename _T1>
double _lam12(_Cl12 _cl, _T1 v211);
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl4 _clRed, _Cl8 _clZip, _T1 _result, _T2 _tmp, _T... vecs);
template<typename _T1, typename... _T>
double _rnz0w(_Cl4 _clRed, _Cl8 _clZip, _T1 _tmp, _T... vecs);
template<typename _T1, typename... _T>
void _zip0(_Cl12 _clZip, _T1 _result, _T... vecs);

template<typename _T1>
void _lam4(_Cl4 _cl, double x3, double y3, _T1 _result) {
    _result = (x3) + (y3);
    
}
double _lam4(_Cl4 _cl, double x3, double y3) {
    double result;
    _lam4(_cl, x3, y3, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1>
void _lam8(_Cl8 _cl, double x17, double x27, _T1 _result) {
    _result = (x17) * (x27);
    
}
double _lam8(_Cl8 _cl, double x17, double x27) {
    double result;
    _lam8(_cl, x17, x27, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename _T2>
void _lam12(_Cl12 _cl, _T1 v211, _T2 _result) {
    View<double*, double, to_list_t<P<3,1>>> v111(userData->at("vec"));
    _rnz0({}, {}, _result, _t11_tmp, v111, v211);
    
}
template<typename _T1>
double _lam12(_Cl12 _cl, _T1 v211) {
    double result;
    _lam12(_cl, v211, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename _T2, typename... _T>
void _rnz0(_Cl4 _clRed, _Cl8 _clZip, _T1 _result, _T2 _tmp, _T... vecs) {
    _lam8(_clZip, vecs[0]..., _result);
    for (int i = 1; i < head<_T...>::size; ++i) {
        _lam8(_clZip, vecs[i]..., _tmp);
        _lam4(_clRed, _result, _tmp, _result);
    }
}
template<typename _T1, typename... _T>
double _rnz0w(_Cl4 _clRed, _Cl8 _clZip, _T1 _tmp, _T... vecs) {
    double result;
    _rnz0(_clRed, _clZip, View<double*, double, to_list_t<>, true>(&result), _tmp, vecs...);
    return result;
}
template<typename _T1, typename... _T>
void _zip0(_Cl12 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam12(_clZip, vecs[i]..., _result[i]);
}
}

View<double*, double, to_list_t<P<2,1>>> evaluator4(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    View<double*, double, to_list_t<P<2,3>, P<3,1>>> _t13(userData->at("mat"));
    _zip0({}, _t14, _t13);
    return _t14;
}
