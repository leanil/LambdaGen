#include "helper.h"
#include "my_sycl.h"
#include "View.h"
#include <map>
#include <string>

View<double*,double,2> evaluator(std::map<std::string, double*> bigVectors){
	View<double*,double,2> result(std::array<size_t,1>{1}, new double[2]);
	buffer_t b_vec(const_cast<const double*>(bigVectors.at("vec")),3);
	buffer_t b_mat(const_cast<const double*>(bigVectors.at("mat")),6);
	buffer_t b_2147482884(result.data, 2);
	buffer_t b_2092764894(16);
	buffer_t b_483997720(6);
	cl::sycl::queue deviceQueue((cl::sycl::gpu_selector()));
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double,3> v_vec(std::array<size_t,1>{1},b_vec.get_access<rw_access>(cgh));
		View<accessor,double,3,2> v_mat(std::array<size_t,2>{1,3},b_mat.get_access<rw_access>(cgh));
		View<accessor,double,3,2> v_483997720(std::array<size_t,2>{2,1},b_483997720.get_access<rw_access>(cgh));
		ParZip([=](View<accessor,double> x){return
		[=](View<accessor,double,2> v){return
		[=](View<accessor,double,2> result, unsigned thread_id){
		Map([=](View<accessor,double> x){return
		[=](View<accessor,double> y){return
		[=](View<accessor,double> result){
		result=x*y;};};}(x),v,result);};};},v_vec,v_mat,v_483997720,8);
	});
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double,3> v_vec(std::array<size_t,1>{1},b_vec.get_access<rw_access>(cgh));
		View<accessor,double,3,2> v_mat(std::array<size_t,2>{1,3},b_mat.get_access<rw_access>(cgh));
		View<accessor,double,2> v_2147482884(std::array<size_t,1>{1},b_2147482884.get_access<rw_access>(cgh));
		View<accessor,double,8,2> v_2092764894(std::array<size_t,2>{2,1},b_2092764894.get_access<rw_access>(cgh));
		View<accessor,double,3,2> v_483997720(std::array<size_t,2>{2,1},b_483997720.get_access<rw_access>(cgh));
		ParReduce([=](View<accessor,double,2> v1){return
		[=](View<accessor,double,2> v2){return
		[=](View<accessor,double,2> result, unsigned thread_id){
		Zip([=](View<accessor,double> x){return
		[=](View<accessor,double> y){return
		[=](View<accessor,double> result){
		result=x+y;};};},v1,v2,result);};};},v_483997720,v_2147482884,v_2092764894,8);
	});
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double,3> v_vec(std::array<size_t,1>{1},b_vec.get_access<rw_access>(cgh));
		View<accessor,double,3,2> v_mat(std::array<size_t,2>{1,3},b_mat.get_access<rw_access>(cgh));
		View<accessor,double,2> v_2147482884(std::array<size_t,1>{1},b_2147482884.get_access<rw_access>(cgh));
		View<accessor,double,8,2> v_2092764894(std::array<size_t,2>{2,1},b_2092764894.get_access<rw_access>(cgh));
		View<accessor,double,3,2> v_483997720(std::array<size_t,2>{2,1},b_483997720.get_access<rw_access>(cgh));
		ParReduceJoin([=](View<accessor,double,2> v1){return
		[=](View<accessor,double,2> v2){return
		[=](View<accessor,double,2> result, unsigned thread_id){
		Zip([=](View<accessor,double> x){return
		[=](View<accessor,double> y){return
		[=](View<accessor,double> result){
		result=x+y;};};},v1,v2,result);};};},v_483997720,v_2147482884,v_2092764894,8);
	});
	return result;
}
