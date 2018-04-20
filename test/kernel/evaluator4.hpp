#include "View.h"
#include <map>
#include <string>

auto evaluator4(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<3,1>>> vec_3(userData["vec"]);
    View<double const*, double, to_list_t<P<2,3>, P<3,1>>> mat_15(userData["mat"]);
    View<double*, double, to_list_t<P<2,1>>> result;
    View<double*, double, to_list_t<>> tmp_4;
    
    for (int idx_1 = 0; idx_1 < 2; ++idx_1) {
        [=](auto v23){
            
            auto v13 = vec_3;
            
            for (int idx_4 = 0; idx_4 < 3; ++idx_4) {
                [=](auto x9, auto y9){
                    
                    
                    
                    
                    tmp_4 = x9 * y9;
                }(v13[idx_4],v23[idx_4]);
                if(idx_4)
                    [=](auto x5, auto y5){
                        
                        
                        
                        
                        result[idx_1] = x5 + y5;
                    }(result[idx_1],tmp_4);
                else
                    result[idx_1] = tmp_4;
            }
        }(mat_15[idx_1]);
    }
    return result;
}
