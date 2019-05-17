#ifndef EVALUATOR9_H
#define EVALUATOR9_H

#include "View.h"
#include <map>
#include <string>

View<double*, double, to_list_t<P<3,1>>> evaluator9(std::map<std::string, double*> const& _userData);

#endif
