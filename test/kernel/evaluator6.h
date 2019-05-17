#ifndef EVALUATOR6_H
#define EVALUATOR6_H

#include "View.h"
#include <map>
#include <string>

View<double*, double, to_list_t<P<3,3>, P<3,1>>> evaluator6(std::map<std::string, double*> const& _userData);

#endif
