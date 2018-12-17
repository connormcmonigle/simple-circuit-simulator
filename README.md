# simple-circuit-simulator

A partially functional templated circuit simulation library

An example usage can be found in example.cpp. A circuit object can be constructed using the make_circuit function. Additional electrical component types can be easily added by specifying additional template parameters.  A number of common component type implentations are included. Circuits are loaded from text files containing a list of components, component parameters and a connectivity matrix.

example.cpp can be compiled with "g++ example.cpp -o example --std=c++17".

# File Format
 <node count>
 <component count>
 <component_type> <parameter> <parameter> ...
 
 <component_type> <parameter> <parameter> ...
  
 <component_type> <parameter> <parameter> ...

 <connectivity matrix>
  
  An example can be found in the test.txt file. The connectivity matrix specifies connections between nodes within the circuit. It is a matrix of integers. Nodes are connected by components of id equal to their count in the component list. Negative values correspond to the negative terminal while positive values correspond to the positive terminal. A zero value within the connectivity matrix corresponds to no connection.
  
 # Known Issues
 
 - Solving systems of equations can result in division by zero with the current implementation.
 - Cycle finding within the circuit graph is currently handled by an unusual/untested algorithm that runs with low time complexity.
 - Uses raw pointers and using namespace std.
 - All code is untested.
