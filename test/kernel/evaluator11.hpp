#include "helper.h"
#include "my_sycl.h"
#include "View.h"
#include <map>
#include <string>

View<double*,double,3> evaluator11(std::map<std::string, double*> bigVectors){
	View<double*,double,3> result(std::array<size_t,1>{1}, new double[3]);
	buffer_t b_a284719151(const_cast<const double*>(bigVectors.at("a")),3);
	buffer_t b_2147482884(result.data, 3);
	buffer_t b_1838073155(3);
	cl::sycl::queue deviceQueue;
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double,3> a284719151(std::array<size_t,1>{1},b_a284719151.get_access<rw_access>(cgh));
		View<accessor,double,3> v_1838073155(std::array<size_t,1>{1},b_1838073155.get_access<rw_access>(cgh));
		ParMap([=](auto x){return
		[=](auto result, unsigned thread_id){
		result=x+1.0;};},a284719151,v_1838073155,4);
	});
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double,3> a284719151(std::array<size_t,1>{1},b_a284719151.get_access<rw_access>(cgh));
		View<accessor,double,3> v_2147482884(std::array<size_t,1>{1},b_2147482884.get_access<rw_access>(cgh));
		View<accessor,double,3> v_1838073155(std::array<size_t,1>{1},b_1838073155.get_access<rw_access>(cgh));
		ParMap([=](auto x){return
		[=](auto y){return
		[=](auto result, unsigned thread_id){
		result=x*y;};};}(2.0),v_1838073155,v_2147482884,4);
	});
	return result;
}
