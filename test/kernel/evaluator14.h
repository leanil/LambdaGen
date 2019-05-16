#ifndef EVALUATOR14_H
#define EVALUATOR14_H

#include "View.h"
#include <map>
#include <string>

View<double*, double, to_list_t<P<3,3>, P<3,1>>> evaluator14(std::map<std::string, double*> const& _userData);

#endif
