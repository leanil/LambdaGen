#ifndef EVALUATOR13_H
#define EVALUATOR13_H

#include "View.h"
#include <map>
#include <string>

View<double*, double, to_list_t<P<4,1>, P<2,4>>> evaluator13(std::map<std::string, double*> const& _userData);

#endif
