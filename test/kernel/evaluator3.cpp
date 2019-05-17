#include "evaluator3.h"

namespace {
struct _Cl3 {
    
};
struct _Cl7 {
    
};

static const std::map<std::string, double*>* userData;


template<typename _T1>
void _lam3(_Cl3 _cl, double y3, _T1 _result);
double _lam3(_Cl3 _cl, double y3);
template<typename _T1>
void _lam7(_Cl7 _cl, double x7, _T1 _result);
double _lam7(_Cl7 _cl, double x7);

template<typename _T1>
void _lam3(_Cl3 _cl, double y3, _T1 _result) {
    _result = (y3) * (y3);
    
}
double _lam3(_Cl3 _cl, double y3) {
    double result;
    _lam3(_cl, y3, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1>
void _lam7(_Cl7 _cl, double x7, _T1 _result) {
    _result = (x7) + (x7);
    
}
double _lam7(_Cl7 _cl, double x7) {
    double result;
    _lam7(_cl, x7, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
}

double evaluator3(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    
    return _lam3({}, _lam7({}, 2.0));
}
