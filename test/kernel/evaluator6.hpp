#include "View.h"
#include <map>
#include <string>

auto evaluator6(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<3,1>>> a_17(userData["a"]);
    View<double const*, double, to_list_t<P<3,1>>> b_18(userData["b"]);
    View<double*, double, to_list_t<P<3,3>, P<3,1>>> result;
    
    [=](auto v1, auto v2){
        
        
        
        for (int idx_3 = 0; idx_3 < 3; ++idx_3) {
            [=](auto x){
                
                
                
                [=](auto x, auto v){
                    
                    
                    
                    for (int idx_7 = 0; idx_7 < 3; ++idx_7) {
                        [=](auto y){
                            
                            auto x1 = x;
                            
                            
                            result[idx_3][idx_7] = x1 * y;
                        }(v[idx_7]);
                    }
                }(x, v2);
            }(v1[idx_3]);
        }
    }(a_17, b_18);
    return result;
}
