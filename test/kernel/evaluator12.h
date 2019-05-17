#ifndef EVALUATOR12_H
#define EVALUATOR12_H

#include "View.h"
#include <map>
#include <string>

View<double*, double, to_list_t<P<4,2>, P<2,1>>> evaluator12(std::map<std::string, double*> const& _userData);

#endif
