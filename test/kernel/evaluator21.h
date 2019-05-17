#ifndef EVALUATOR21_H
#define EVALUATOR21_H

#include "View.h"
#include <map>
#include <string>

View<double*, double, to_list_t<P<3,3>, P<3,1>>> evaluator21(std::map<std::string, double*> const& _userData);

#endif
