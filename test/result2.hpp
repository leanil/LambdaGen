#include "helper.h"
#include "View.h"
#include <map>
#include <string>
#include <vector>

auto evaluator(std::map<std::string, double*> bigVectors){
	View<double> s2147482884;
	View<double> s1838073155;
	s1838073155=4.0*3.0,
	([&](const auto& x){return
	[&](const auto& y){return
	[&](const auto& result){
	result=x+y;};};}(5.0))(s1838073155)(s2147482884);
	return s2147482884;
}
