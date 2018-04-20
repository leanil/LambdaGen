#include "View.h"
#include <map>
#include <string>

auto evaluator7(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<2,3>, P<3,1>>> mat_21(userData["mat"]);
    View<double const*, double, to_list_t<P<2,1>, P<3,2>>> mat_22(userData["mat"]);
    View<double*, double, to_list_t<P<2,2>, P<2,1>>> result;
    View<double*, double, to_list_t<>> tmp_8;
    
    [=](auto m12, auto m22){
        
        
        
        for (int idx_3 = 0; idx_3 < 2; ++idx_3) {
            [=](auto v4){
                
                
                
                for (int idx_5 = 0; idx_5 < 2; ++idx_5) {
                    [=](auto v27){
                        
                        auto v17 = v4;
                        
                        for (int idx_8 = 0; idx_8 < 3; ++idx_8) {
                            [=](auto x13, auto y13){
                                
                                
                                
                                
                                tmp_8 = x13 * y13;
                            }(v17[idx_8],v27[idx_8]);
                            if(idx_8)
                                [=](auto x9, auto y9){
                                    
                                    
                                    
                                    
                                    result[idx_3][idx_5] = x9 + y9;
                                }(result[idx_3][idx_5],tmp_8);
                            else
                                result[idx_3][idx_5] = tmp_8;
                        }
                    }(m22[idx_5]);
                }
            }(m12[idx_3]);
        }
    }(mat_21, mat_22);
    return result;
}
