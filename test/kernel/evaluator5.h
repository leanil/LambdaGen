#ifndef EVALUATOR5_H
#define EVALUATOR5_H

#include "View.h"
#include <map>
#include <string>

View<double*, double, to_list_t<P<2,1>>> evaluator5(std::map<std::string, double*> const& _userData);

#endif
