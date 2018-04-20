#include "View.h"
#include <map>
#include <string>

auto evaluator3(std::map<std::string, double*> userData) {
    
    View<double*, double, to_list_t<>> result;
    View<double*, double, to_list_t<>> v_6;
    
    [=](auto x7){
        
        
        
        
        v_6 = x7 + x7;
    }(2.0);
    [=](auto y2){
        
        
        
        
        result = y2 * y2;
    }(v_6);
    return result;
}
