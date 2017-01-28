#include "helper.h"
#include "View.h"
#include <map>
#include <string>
#include <vector>

auto evaluator(std::map<std::string, double*> bigVectors){
	View<double> s2147482884;
	View<double> s1838073155;
	[&](const auto& x){return
	[&](const auto& result){
	result=x+x;};}(2.0)(s1838073155),
	[&](const auto& y){return
	[&](const auto& result){
	result=y*y;};}(s1838073155)(s2147482884);
	return s2147482884;
}
