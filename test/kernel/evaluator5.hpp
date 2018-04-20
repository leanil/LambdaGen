#include "View.h"
#include <map>
#include <string>

auto evaluator5(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<3,1>>> vec_18(userData["vec"]);
    View<double const*, double, to_list_t<P<3,1>, P<2,3>>> mat_19(userData["mat"]);
    View<double*, double, to_list_t<P<2,1>>> result;
    View<double*, double, to_list_t<P<2,1>>> tmp_1;
    
    for (int idx_1 = 0; idx_1 < 3; ++idx_1) {
        [=](auto x, auto v){
            
            
            
            for (int idx_11 = 0; idx_11 < 2; ++idx_11) {
                [=](auto y){
                    
                    auto x1 = x;
                    
                    
                    tmp_1[idx_11] = x1 * y;
                }(v[idx_11]);
            }
        }(vec_18[idx_1],mat_19[idx_1]);
        if(idx_1)
            [=](auto v1, auto v2){
                
                
                
                for (int idx_3 = 0; idx_3 < 2; ++idx_3) {
                    [=](auto x, auto y){
                        
                        
                        
                        
                        result[idx_3] = x + y;
                    }(v1[idx_3],v2[idx_3]);
                }
            }(result,tmp_1);
        else
            result = tmp_1;
    }
    return result;
}
