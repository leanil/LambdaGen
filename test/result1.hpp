#include "helper.h"
#include "my_sycl.h"
#include "View.h"
#include <map>
#include <string>

View<double*,double> evaluator(std::map<std::string, double*> bigVectors){
	View<double*,double> result(std::array<size_t,0>{}, new double[1]);
	buffer_t b_2147482884(result.data, 1);
	buffer_t b_1645233509(1);
	cl::sycl::queue deviceQueue;
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double> v_2147482884(std::array<size_t,0>{},b_2147482884.get_access<rw_access>(cgh));
		View<accessor,double> v_1645233509(std::array<size_t,0>{},b_1645233509.get_access<rw_access>(cgh));
		act_cgh->single_task<class SingleKernel>([=] () mutable {
			v_1645233509=2.0+3.0,
			v_2147482884=v_1645233509*4.0;
		});
	});
	return result;
}
