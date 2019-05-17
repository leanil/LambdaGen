#ifndef EVALUATOR17_H
#define EVALUATOR17_H

#include "View.h"
#include <map>
#include <string>

View<double*, double, to_list_t<P<4,2>, P<2,1>>> evaluator17(std::map<std::string, double*> const& _userData);

#endif
