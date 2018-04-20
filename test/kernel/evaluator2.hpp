#include "View.h"
#include <map>
#include <string>

auto evaluator2(std::map<std::string, double*> userData) {
    
    View<double*, double, to_list_t<>> result;
    
    [=](auto x2, auto y2){
        
        
        
        
        result = x2 + y2;
    }(5.0, 12.0);
    return result;
}
