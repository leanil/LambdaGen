#ifndef EVALUATOR10_H
#define EVALUATOR10_H

#include "View.h"
#include <map>
#include <string>

View<double*, double, to_list_t<P<3,1>>> evaluator10(std::map<std::string, double*> const& _userData);

#endif
