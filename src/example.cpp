#include "sim.hpp"
#include<iostream>
#include<fstream>
#include<vector>
#include<tuple>

int main(){
  std::fstream file("test.txt");
  auto c = circuit::make_circuit<double, circuit::resistor, circuit::battery, circuit::capacitor, circuit::inductor>(file);
	for(size_t i(0); i < 100; ++i){
  	std::vector<std::tuple<double, double>> vec = c.compute_IV();
		for(auto i : vec){
			std::cout << "(" << std::get<0>(i) << ", " << std::get<1>(i) << ")" << " ";
		}
		std::cout << std::endl;
	}
}
