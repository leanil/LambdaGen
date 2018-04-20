#include "View.h"
#include <map>
#include <string>

auto evaluator8(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<4,2>, P<2,1>>> mat8_32(userData["mat8"]);
    View<double const*, double, to_list_t<P<2,12>, P<4,1>, P<3,4>>> tens_33(userData["tens"]);
    View<double*, double, to_list_t<P<2,6>, P<3,2>, P<2,1>>> result;
    View<double*, double, to_list_t<P<3,2>, P<2,1>>> tmp_3;
    
    for (int idx_1 = 0; idx_1 < 2; ++idx_1) {
        [=](auto m){
            
            
            
            for (int idx_3 = 0; idx_3 < 4; ++idx_3) {
                [=](auto v1, auto v2){
                    
                    
                    
                    for (int idx_17 = 0; idx_17 < 3; ++idx_17) {
                        [=](auto x){
                            
                            
                            
                            [=](auto x, auto v){
                                
                                
                                
                                for (int idx_21 = 0; idx_21 < 2; ++idx_21) {
                                    [=](auto y){
                                        
                                        auto x1 = x;
                                        
                                        
                                        tmp_3[idx_17][idx_21] = x1 * y;
                                    }(v[idx_21]);
                                }
                            }(x, v2);
                        }(v1[idx_17]);
                    }
                }(m[idx_3],mat8_32[idx_3]);
                if(idx_3)
                    [=](auto m1, auto m2){
                        
                        
                        
                        for (int idx_5 = 0; idx_5 < 3; ++idx_5) {
                            [=](auto v1, auto v2){
                                
                                
                                
                                for (int idx_7 = 0; idx_7 < 2; ++idx_7) {
                                    [=](auto x, auto y){
                                        
                                        
                                        
                                        
                                        result[idx_1][idx_5][idx_7] = x + y;
                                    }(v1[idx_7],v2[idx_7]);
                                }
                            }(m1[idx_5],m2[idx_5]);
                        }
                    }(result[idx_1],tmp_3);
                else
                    result[idx_1] = tmp_3;
            }
        }(tens_33[idx_1]);
    }
    return result;
}
