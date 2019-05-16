#include "evaluator12.h"

namespace {
struct _Cl16 {
    
};
struct _Cl15 {
    double q16;
    double x16;
};
struct _Cl7 {
    double x16;
    double y15;
};

static const std::map<std::string, double*>* userData;


template<typename _T1>
void _lam7(_Cl7 _cl, double z7, _T1 _result);
double _lam7(_Cl7 _cl, double z7);
template<typename _T1>
void _lam15(_Cl15 _cl, double y15, _T1 _result);
double _lam15(_Cl15 _cl, double y15);
_Cl15 _lam16(_Cl16 _cl, double x16);

template<typename _T1>
void _lam7(_Cl7 _cl, double z7, _T1 _result) {
    _result = ((_cl.x16) + (_cl.y15)) + (z7);
    
}
double _lam7(_Cl7 _cl, double z7) {
    double result;
    _lam7(_cl, z7, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
template<typename _T1>
void _lam15(_Cl15 _cl, double y15, _T1 _result) {
    double p15 = 8.0;
    _Cl7 h15 = {_cl.x16, y15};
    _result = ((_lam7(h15, 6.0)) * (p15)) + (_cl.q16);
    
}
double _lam15(_Cl15 _cl, double y15) {
    double result;
    _lam15(_cl, y15, View<double*, double, to_list_t<>, true>(&result));
    return result;
}
_Cl15 _lam16(_Cl16 _cl, double x16) {
    double q16 = 8.0;
    return {q16, x16};
}
}

double evaluator12(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    
    return _lam15(_lam16({}, 2.0), 3.0);
}
