#ifndef EVALUATOR8_H
#define EVALUATOR8_H

#include "View.h"
#include <map>
#include <string>

View<double*, double, to_list_t<P<2,6>, P<3,2>, P<2,1>>> evaluator8(std::map<std::string, double*> const& _userData);

#endif
