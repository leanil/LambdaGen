#include "View.h"
#include <map>
#include <string>

auto evaluator4(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<3,1>>> vec_3(userData["vec"]);
    View<double const*, double, to_list_t<P<2,3>, P<3,1>>> mat_15(userData["mat"]);
    View<double*, double, to_list_t<P<2,1>>> result;
    View<double*, double, to_list_t<>> tmp_4;
    
    for (int idx_1 = 0; idx_1 < 2; ++idx_1) {
        [=](auto v2){
            
            auto v1 = vec_3;
            
            for (int idx_4 = 0; idx_4 < 3; ++idx_4) {
                [=](auto x1, auto y){
                    
                    
                    
                    
                    tmp_4 = x1 * y;
                }(v1[idx_4],v2[idx_4]);
                if(idx_4)
                    [=](auto x, auto y){
                        
                        
                        
                        
                        result[idx_1] = x + y;
                    }(result[idx_1],tmp_4);
                else
                    result[idx_1] = tmp_4;
            }
        }(mat_15[idx_1]);
    }
    return result;
}
