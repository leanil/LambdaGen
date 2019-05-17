#ifndef EVALUATOR4_H
#define EVALUATOR4_H

#include "View.h"
#include <map>
#include <string>

View<double*, double, to_list_t<P<2,1>>> evaluator4(std::map<std::string, double*> const& _userData);

#endif
