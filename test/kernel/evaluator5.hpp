#include "helper.h"
#include "my_sycl.h"
#include "View.h"
#include <map>
#include <string>

View<double*,double,2> evaluator5(std::map<std::string, double*> bigVectors){
	View<double*,double,2> result(std::array<size_t,1>{1}, new double[2]);
	buffer_t b_vec471984148(const_cast<const double*>(bigVectors.at("vec")),3);
	buffer_t b_mat1680077988(const_cast<const double*>(bigVectors.at("mat")),6);
	buffer_t b_2147482884(result.data, 2);
	buffer_t b_1645233509(8);
	buffer_t b_1838073155(6);
	cl::sycl::queue deviceQueue;
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double,3> vec471984148(std::array<size_t,1>{1},b_vec471984148.get_access<rw_access>(cgh));
		View<accessor,double,3,2> mat1680077988(std::array<size_t,2>{1,3},b_mat1680077988.get_access<rw_access>(cgh));
		View<accessor,double,3,2> v_1838073155(std::array<size_t,2>{2,1},b_1838073155.get_access<rw_access>(cgh));
		ParZip([=](auto x){return
		[=](auto v){return
		[=](auto result, unsigned thread_id){return
		Map([=](auto x){return
		[=](auto y){return
		[=](auto result){return
		result=x*y;};};}(x),v,result);};};},vec471984148,mat1680077988,v_1838073155,4);
	});
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double,3> vec471984148(std::array<size_t,1>{1},b_vec471984148.get_access<rw_access>(cgh));
		View<accessor,double,3,2> mat1680077988(std::array<size_t,2>{1,3},b_mat1680077988.get_access<rw_access>(cgh));
		View<accessor,double,2> v_2147482884(std::array<size_t,1>{1},b_2147482884.get_access<rw_access>(cgh));
		View<accessor,double,4,2> v_1645233509(std::array<size_t,2>{2,1},b_1645233509.get_access<rw_access>(cgh));
		View<accessor,double,3,2> v_1838073155(std::array<size_t,2>{2,1},b_1838073155.get_access<rw_access>(cgh));
		ParReduce([=](auto v1){return
		[=](auto v2){return
		[=](auto result, unsigned thread_id){return
		Zip([=](auto x){return
		[=](auto y){return
		[=](auto result){return
		result=x+y;};};},v1,v2,result);};};},v_1838073155,v_2147482884,v_1645233509,4);
	});
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double,3> vec471984148(std::array<size_t,1>{1},b_vec471984148.get_access<rw_access>(cgh));
		View<accessor,double,3,2> mat1680077988(std::array<size_t,2>{1,3},b_mat1680077988.get_access<rw_access>(cgh));
		View<accessor,double,2> v_2147482884(std::array<size_t,1>{1},b_2147482884.get_access<rw_access>(cgh));
		View<accessor,double,4,2> v_1645233509(std::array<size_t,2>{2,1},b_1645233509.get_access<rw_access>(cgh));
		View<accessor,double,3,2> v_1838073155(std::array<size_t,2>{2,1},b_1838073155.get_access<rw_access>(cgh));
		ParReduceJoin([=](auto v1){return
		[=](auto v2){return
		[=](auto result, unsigned thread_id){return
		Zip([=](auto x){return
		[=](auto y){return
		[=](auto result){return
		result=x+y;};};},v1,v2,result);};};},v_1838073155,v_2147482884,v_1645233509,4);
	});
	return result;
}
