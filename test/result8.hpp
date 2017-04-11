#include "helper.h"
#include "my_sycl.h"
#include "View.h"
#include <map>
#include <string>

View<double*,double,3,3> evaluator(std::map<std::string, double*> bigVectors){
	View<double*,double,3,3> result(std::array<size_t,2>{3,1}, new double[9]);
	buffer_t b_a(const_cast<const double*>(bigVectors.at("a")),3);
	buffer_t b_b(const_cast<const double*>(bigVectors.at("b")),3);
	buffer_t b_2147482884(result.data, 9);
	cl::sycl::queue deviceQueue((cl::sycl::gpu_selector()));
	[=](View<accessor,double,3> v1){return
	[=](View<accessor,double,3> v2){return
	[=](View<accessor,double,3,3> result){
        deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		    act_cgh = &cgh;
		    ParMap([=](View<accessor,double> x){return
		    [=](View<accessor,double,3> result, unsigned thread_id){
		    [=](View<accessor,double> x){return
		    [=](View<accessor,double,3> v){return
		    [=](View<accessor,double,3> result){
		    Map([=](View<accessor,double> x){return
		    [=](View<accessor,double> y){return
		    [=](View<accessor,double> result){
		    result=x*y;};};}(x),v,result);};};}(x)(v2)(result);};},v1,result,8);
	    });
	;};};}(v_a)(v_b)(v_2147482884)
	return result;
}
