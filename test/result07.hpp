#include "helper.h"
#include "my_sycl.h"
#include "View.h"
#include <map>
#include <string>

View<double*,double,2,2> evaluator(std::map<std::string, double*> bigVectors){
	View<double*,double,2,2> result(std::array<size_t,2>{2,1}, new double[4]);
	buffer_t b_mat844138389(const_cast<const double*>(bigVectors.at("mat")),6);
	buffer_t b_mat1838073155(const_cast<const double*>(bigVectors.at("mat")),6);
	buffer_t b_2147482884(result.data, 4);
	buffer_t b_226741871(3);
	cl::sycl::queue deviceQueue;
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double,2,3> mat844138389(std::array<size_t,2>{3,1},b_mat844138389.get_access<rw_access>(cgh));
		View<accessor,double,2,3> mat1838073155(std::array<size_t,2>{1,2},b_mat1838073155.get_access<rw_access>(cgh));
		View<accessor,double,2,2> v_2147482884(std::array<size_t,2>{2,1},b_2147482884.get_access<rw_access>(cgh));
		View<accessor,double,3> v_226741871(std::array<size_t,1>{1},b_226741871.get_access<rw_access>(cgh));
		act_cgh->single_task<class SingleKernel>([=] () mutable {
			[=](View<accessor,double,2,3> m1){return
			[=](View<accessor,double,2,3> m2){return
			[=](View<accessor,double,2,2> result){
			Map([=](View<accessor,double,3> v){return
			[=](View<accessor,double,2> result){
			Map([=](View<accessor,double,3> v1){return
			[=](View<accessor,double,3> v2){return
			[=](View<accessor,double> result){
			Zip([=](double x){return
			[=](double y){return
			[=](View<accessor,double> result){
			result=x*y;};};},v1,v2,v_226741871),
			Reduce([=](double x){return
			[=](double y){return
			[=](View<accessor,double> result){
			result=x+y;};};},v_226741871,result);};};}(v),m2,result);};},m1,result);};};}(mat844138389)(mat1838073155)(v_2147482884);
		});
	});
	return result;
}
