#include "evaluator2.h"

namespace {
struct _Cl3 {
    
};

static const std::map<std::string, double*>* userData;


template<typename _T1>
void _lam3(_Cl3 _cl, double x3, double y3, _T1 _result);
double _lam3(_Cl3 _cl, double x3, double y3);

template<typename _T1>
void _lam3(_Cl3 _cl, double x3, double y3, _T1 _result) {
    _result = (x3) + (y3);
    
}
double _lam3(_Cl3 _cl, double x3, double y3) {
    double result;
    _lam3(_cl, x3, y3, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
}

double evaluator2(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    
    return _lam3({}, 5.0, (4.0) * (3.0));
}
