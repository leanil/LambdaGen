#include "helper.h"
#include "View.h"
#include <map>
#include <string>
#include <vector>

auto evaluator(std::map<std::string, double*> bigVectors){
	View<double,Pair<2,1>> s2147482884;
	View<double,Pair<8,2>,Pair<2,1>> s2092764894;
	View<double,Pair<3,2>,Pair<2,1>> s483997720;
	ParZip([&](const auto& x){return
	[&](const auto& v){return
	[&](const auto& result, unsigned thread_id){
	Map(([&](const auto& x){return
	[&](const auto& y){return
	[&](const auto& result){
	result=x*y;};};}(x)),v,result);};};},View<double,Pair<3,1>>(bigVectors.at("vec")),View<double,Pair<3,1>,Pair<2,3>>(bigVectors.at("mat")),s483997720,8),
	ParReduce([&](const auto& v1){return
	[&](const auto& v2){return
	[&](const auto& result, unsigned thread_id){
	Zip([&](const auto& x){return
	[&](const auto& y){return
	[&](const auto& result){
	result=x+y;};};},v1,v2,result);};};},s483997720,s2147482884,s2092764894,8);
	return s2147482884;
}
