#include "View.h"
#include <map>
#include <string>

auto evaluator3(std::map<std::string, double*> userData) {
    
    View<double*, double, to_list_t<>> result;
    View<double*, double, to_list_t<>> v_6;
    
    [=](auto x){
        
        
        
        
        v_6 = x + x;
    }(2.0);
    [=](auto y){
        
        
        
        
        result = y * y;
    }(v_6);
    return result;
}
