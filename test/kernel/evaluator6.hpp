#include "helper.h"
#include "my_sycl.h"
#include "View.h"
#include <map>
#include <string>

View<double*,double,3,3> evaluator6(std::map<std::string, double*> bigVectors){
	View<double*,double,3,3> result(std::array<size_t,2>{3,1}, new double[9]);
	buffer_t b_a844138389(const_cast<const double*>(bigVectors.at("a")),3);
	buffer_t b_b1838073155(const_cast<const double*>(bigVectors.at("b")),3);
	buffer_t b_2147482884(result.data, 9);
	cl::sycl::queue deviceQueue;
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double,3> a844138389(std::array<size_t,1>{1},b_a844138389.get_access<rw_access>(cgh));
		View<accessor,double,3> b1838073155(std::array<size_t,1>{1},b_b1838073155.get_access<rw_access>(cgh));
		View<accessor,double,3,3> v_2147482884(std::array<size_t,2>{3,1},b_2147482884.get_access<rw_access>(cgh));
		act_cgh->single_task<class SingleKernel>([=] () mutable {
			[=](View<accessor,double,3> v1){return
			[=](View<accessor,double,3> v2){return
			[=](View<accessor,double,3,3> result){
			Map([=](double x){return
			[=](View<accessor,double,3> result){
			[=](double x){return
			[=](View<accessor,double,3> v){return
			[=](View<accessor,double,3> result){
			Map([=](double x){return
			[=](double y){return
			[=](View<accessor,double> result){
			result=x*y;};};}(x),v,result);};};}(x)(v2)(result);};},v1,result);};};}(a844138389)(b1838073155)(v_2147482884);
		});
	});
	return result;
}
