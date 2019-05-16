#ifndef EVALUATOR7_H
#define EVALUATOR7_H

#include "View.h"
#include <map>
#include <string>

View<double*, double, to_list_t<P<2,2>, P<2,1>>> evaluator7(std::map<std::string, double*> const& _userData);

#endif
