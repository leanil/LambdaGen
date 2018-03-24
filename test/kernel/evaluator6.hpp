#include "helper.h"
#include "my_sycl.h"
#include "View.h"
#include <map>
#include <string>

View<double*,double,3,3> evaluator6(std::map<std::string, double*> bigVectors){
	View<double*,double,3,3> result(std::array<size_t,2>{3,1}, new double[9]);
	buffer_t b_a898856380(const_cast<const double*>(bigVectors.at("a")),3);
	buffer_t b_b931609811(const_cast<const double*>(bigVectors.at("b")),3);
	buffer_t b_2147482884(result.data, 9);
	cl::sycl::queue deviceQueue;
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double,3> a898856380(std::array<size_t,1>{1},b_a898856380.get_access<rw_access>(cgh));
		View<accessor,double,3> b931609811(std::array<size_t,1>{1},b_b931609811.get_access<rw_access>(cgh));
		View<accessor,double,3,3> v_2147482884(std::array<size_t,2>{3,1},b_2147482884.get_access<rw_access>(cgh));
		act_cgh->single_task<class SingleKernel>([=] () mutable {
			[=](auto v1){return
			[=](auto v2){return
			[=](auto result){return
			Map([=](auto x){return
			[=](auto result){return
			[=](auto x){return
			[=](auto v){return
			[=](auto result){return
			Map([=](auto x){return
			[=](auto y){return
			[=](auto result){return
			result=x*y;};};}(x),v,result);};};}(x)(v2)(result);};},v1,result);};};}(a898856380)(b931609811)(v_2147482884);
		});
	});
	return result;
}
