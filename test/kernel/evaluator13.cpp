#include "evaluator13.h"

namespace {
struct _Cl18 {
    
};
struct _Cl3 {
    double x18;
};
struct _Cl17 {
    _Cl3 f18;
};
struct _Cl5 {
    _Cl3 f18;
};
struct _Cl7 {
    _Cl3 f18;
};

static const std::map<std::string, double*>* userData;


template<typename _T1>
void _lam3(_Cl3 _cl, double y3, _T1 _result);
double _lam3(_Cl3 _cl, double y3);
_Cl3 _lam5(_Cl5 _cl, double z5);
_Cl3 _lam7(_Cl7 _cl, double z7);
template<typename _T1>
void _lam17(_Cl17 _cl, double y17, _T1 _result);
double _lam17(_Cl17 _cl, double y17);
_Cl17 _lam18(_Cl18 _cl, double x18);

template<typename _T1>
void _lam3(_Cl3 _cl, double y3, _T1 _result) {
    _result = (_cl.x18) + (y3);
    
}
double _lam3(_Cl3 _cl, double y3) {
    double result;
    _lam3(_cl, y3, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
_Cl3 _lam5(_Cl5 _cl, double z5) {
    
    return _cl.f18;
}
_Cl3 _lam7(_Cl7 _cl, double z7) {
    
    return _cl.f18;
}
template<typename _T1>
void _lam17(_Cl17 _cl, double y17, _T1 _result) {
    _Cl5 g17 = {_cl.f18};
    _Cl7 h17 = {_cl.f18};
    _lam3(_lam5(g17, 1.0), _lam3(_lam7(h17, 1.0), 1.0), _result);
    
}
double _lam17(_Cl17 _cl, double y17) {
    double result;
    _lam17(_cl, y17, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
_Cl17 _lam18(_Cl18 _cl, double x18) {
    _Cl3 f18 = {x18};
    return {f18};
}
}

double evaluator13(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    
    return _lam17(_lam18({}, 1.0), 1.0);
}
