#include "View.h"
#include <map>
#include <string>

auto evaluator6(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<3,1>>> a_17(userData["a"]);
    View<double const*, double, to_list_t<P<3,1>>> b_18(userData["b"]);
    View<double*, double, to_list_t<P<3,3>, P<3,1>>> result;
    
    [=](auto v12, auto v22){
        
        
        
        for (int idx_3 = 0; idx_3 < 3; ++idx_3) {
            [=](auto x4){
                
                
                
                [=](auto x6, auto v6){
                    
                    
                    
                    for (int idx_7 = 0; idx_7 < 3; ++idx_7) {
                        [=](auto y9){
                            
                            auto x9 = x6;
                            
                            
                            result[idx_3][idx_7] = x9 * y9;
                        }(v6[idx_7]);
                    }
                }(x4, v22);
            }(v12[idx_3]);
        }
    }(a_17, b_18);
    return result;
}
