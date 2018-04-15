#include "View.h"
#include <map>
#include <string>

auto evaluator1(std::map<std::string, double*> userData) {
    
    View<double*, double, to_list_t<>> result;
    result = 20.0;
    return result;
}
