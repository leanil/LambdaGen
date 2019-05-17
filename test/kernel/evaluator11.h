#ifndef EVALUATOR11_H
#define EVALUATOR11_H

#include "View.h"
#include <map>
#include <string>

View<double*, double, to_list_t<P<3,1>>> evaluator11(std::map<std::string, double*> const& _userData);

#endif
