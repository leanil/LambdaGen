#include "evaluator1.h"

namespace {


static const std::map<std::string, double*>* userData;





}

double evaluator1(std::map<std::string, double*> const& _userData) {
    userData = &_userData;
    
    return ((2.0) + (3.0)) * (4.0);
}
