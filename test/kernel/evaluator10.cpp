#include "evaluator10.h"

namespace {
struct _Cl6 {
    
};

static const std::map<std::string, double*>* userData;
View<double*, double, to_list_t<P<3,1>>> _t8;

template<typename _T1>
void _lam6(_Cl6 _cl, double y5, _T1 _result);
double _lam6(_Cl6 _cl, double y5);
template<typename _T1, typename... _T>
void _zip0(_Cl6 _clZip, _T1 _result, _T... vecs);

template<typename _T1>
void _lam6(_Cl6 _cl, double y5, _T1 _result) {
    double x5 = 3.0;
    _result = ((y5) * (0.0)) + (x5);
    
}
double _lam6(_Cl6 _cl, double y5) {
    double result;
    _lam6(_cl, y5, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1, typename... _T>
void _zip0(_Cl6 _clZip, _T1 _result, _T... vecs) {
    for (int i = 0; i < _result.size; ++i)
        _lam6(_clZip, vecs[i]..., _result[i]);
}
}

View<double*, double, to_list_t<P<3,1>>> evaluator10(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    View<double*, double, to_list_t<P<3,1>>> _t7(userData->at("a"));
    _zip0({}, _t8, _t7);
    return _t8;
}
