#include<iostream>
#include<fstream>
#include<cmath>
#include<vector>
#include<unordered_map>
#include<set>
#include<functional>
#include<tuple>
#include<algorithm>
#include<random>
#include<utility>

#define dt 0.0001
#define LOW 0.001

namespace helper{

  using namespace std;

  template<typename T, typename ... Ts> function<T*(Ts...)> make_callable(T*(*func)(Ts ...)){
    return [](Ts ... ts){ return new T(forward<Ts>(ts)...); };
  }

  template<typename ... Ts> struct type_count{
	  const static size_t value = 0;
  };

  template<typename T, typename ... Ts> struct type_count<T, Ts...>{
	  const static size_t value = type_count<Ts...>::value + 1;
  };

  template<typename I> auto extract_tuple(I& in){
	  return make_tuple();
  }

  template<typename I, typename T, typename ... Ts> auto extract_tuple(I& in){
	  T i; in >> i; 
	  return tuple_cat(make_tuple(i), extract_tuple<I, Ts...>(in));
  }

  template<typename F, typename Tuple, size_t ... Is> auto send_impl(F f, Tuple extracted_tuple, index_sequence<Is ... >){
	  return f(get<Is>(extracted_tuple)...);
  }

  template<typename I, typename T, typename ... Ts> T send(I& in, function<T(Ts...)> f){
	  return send_impl(f, extract_tuple<I, Ts...>(in), make_index_sequence< sizeof ... (Ts) >{});
  }
}

namespace mat{

  using namespace std;

  template<typename R> struct matrix{
	  vector<vector<R>> m;
	  size_t height, width;
  
	  vector<R>& operator[](size_t i){
	  	return m[i];
	  }
  
	  template<typename F> matrix<R>& apply(F f){
	  	for(auto& i : m){
	  		transform(i.begin(), i.end(), i.begin(), f);
	  	}
	  	return *this;
	  }
	  
	  matrix<R>& flip_row(tuple<size_t, size_t> in){
	    swap(m[get<0>(in)], m[get<1>(in)]);
	    return *this;
	  }
	  
	  matrix<R>& mul_row(tuple<size_t, R> in){
	    auto mul = [in](R val){ return get<1>(in) * val; };
	    transform(m[get<0>(in)].begin(), m[get<0>(in)].end(),
	              m[get<0>(in)].begin(), mul);
	    return *this;
	  }
	  
	  matrix<R>& sub_row(tuple<size_t, R> a, tuple<size_t, R> b){
	    auto it = mul_row(a)[get<0>(b)].begin();
	    for(R& elem : m[get<0>(a)]){
	      elem -= *it * get<1>(b);
	      ++it;
	    }
	    return *this;
	  }
	  
	  matrix<R>& row_reduce(){
	    for(size_t i(0); i < m.size(); ++i){
	      for(size_t j(0); j < i; ++j){
	        if(m[i][j] != 0){
	          sub_row(make_tuple(i, R(1)),make_tuple(j, m[i][j]));
	        }
	      }
	      mul_row(make_tuple(i, R(1) / m[i][i]));
	    }
	    return *this;
	  }
	  
	  void print(){
	    for(auto i : m){
	      cout << "{ ";
	      for(auto j : i) cout << j << " ";
	      cout << " }" << endl;
	    }
	  }  
  
	  matrix(size_t y, size_t x) : m(vector<vector<R>>(y, vector<R>(x, R(LOW)))) {
	  	height = y;
	  	width = x;
	  }
  };

  template<typename T> matrix<T> operator*(const matrix<T>& a, const matrix<T>& b){
	  if(a.width != b.height) throw "dimension_error";
	  matrix<T> mat(a.height, b.width);
	  for(size_t i(0); i < a.height; ++i){
		  for(size_t j(0); j < b.width; ++j){
		  	for(size_t k(0); k < a.width; ++k){
		  		mat[i][j] += a[i][k] * b[k][j];
		  	}
		  }
	  }
	  return mat;
  }

  template<typename T> matrix<T> operator+(const matrix<T>& a, const matrix<T>& b){
	  if(a.width != b.width || a.height != b.height) throw "dimension_error";
	  matrix<T> mat = a;
	  for(size_t i(0); i < a.height; ++i){
	  	for(size_t j(0); i < a.width; ++j){
	  		mat[i][j] += b[i][j];
	  	}
  	}
	  return mat;
  }
  
  template<typename T> T dot(const vector<T>& a, const vector<T>& b){
    if(a.size() != b.size()) throw "dimension_error";
    T result(0);
    auto a_it = a.begin();
    auto b_it = b.begin();
    for(; a_it != a.end(); ++a_it, ++b_it){
      result += *a_it * *b_it;
    }
    return result;
  }
  
  template<typename T> vector<T> solve(matrix<T> combined){
    if(combined.width != combined.height + 1) throw "dimension_error";
    combined.row_reduce();
    vector<T> result(combined.m.size(), T(0));
    vector<T> rhs{};
    for(auto& vec : combined.m){
      rhs.push_back(vec.back());
      vec.pop_back(); 
    }
    auto rhs_it = rhs.rbegin();
    auto result_it = result.rbegin();
    auto it = combined.m.rbegin();    
    for(; it != combined.m.rend(); ++it, ++rhs_it, ++result_it){
      *result_it = *rhs_it - dot(*it, result);
    }
    return result;
  }

  template<typename T> void vec_print(const vector<T>& in){
    cout << "{ ";
    for(const T j : in){
      cout << j << " ";
    }
    cout << "}";
  }

}

namespace cyclical{

  using namespace std;

	template<typename T> struct vector_modifier{
		struct iterator{
			size_t index;
			vector_modifier<T>& c;
			iterator& operator++(){++index; return *this;}
			iterator& operator--(){--index; return *this;}
			bool operator==(const iterator& in){
				return index == in.index;
			}
			bool operator!=(const iterator& in){
				return index != in.index;
			}
			T operator*(){
				return c[index];
			}
			iterator(vector_modifier<T>& body, size_t i) : c(body), index(i){}
		};
		iterator begin(){
			return iterator(*this, 0);
		}
		iterator end(){
			return iterator(*this, local.size() - 1);
		}
		vector<T>& local;
		size_t index;
		T replace;
		T operator[](size_t i){
			if(i == index) return replace;
			return local[i];
		}
		vector_modifier(vector<T>& input, size_t i, T&& r) : local(input){
			replace = r; index = i;
		}
	};

	template<typename It> vector<vector<size_t>> cycles(It first, It last){
		vector<vector<size_t>> result = {};
		vector<tuple<bool, vector<size_t>>> store(distance(first, last), {false, {}});

		auto add = [&result, &store](size_t loc,  auto line){
			size_t i(0);
			for(bool b : line){
				if(b){
					if(get<0>(store[i])){
						size_t j(0);
						for(;(j + 1 < get<1>(store[i]).size()) && (j + 1 < get<1>(store[loc]).size())
									&& get<1>(store[i])[j + 1] == get<1>(store[loc])[j + 1]; ++j);
						result.push_back(vector<size_t>(get<1>(store[loc]).begin() + j, get<1>(store[loc]).end()));
						result.back().push_back(loc); result.back().push_back(i);
						result.back().insert(result.back().end(), get<1>(store[i]).rbegin(), get<1>(store[i]).rend() - j);
					}else{
						get<0>(store[i]) = true;
						get<1>(store[i]) = get<1>(store[loc]);
						get<1>(store[i]).push_back(loc);
					}
				}
				++i;
			}
		};

		auto done = [&store](){
			for(auto& elem : store){
				if(!get<0>(elem)) return false;
			}
			return true;
		};

		vector<tuple<bool, vector<size_t>>> prev = store;
		add(0, *first);
		while(prev != store){
			prev = store;
			auto i = first;
			for(auto& elem : store){
				if(get<0>(elem)){
					add(distance(first, i), vector_modifier<bool>(*i, get<1>(elem).back(), false));
				}
				++i;
			}
		}
		return result;
	}
}


namespace circuit{

  using namespace std;

  template<typename R> struct component{
    virtual R get_imp(bool b){ return R(0); };
    virtual R get_rise(){ return R(0); };
    virtual void set_IV(R I, R V){ return; };
    virtual void set_time(R t){ return; };
  }; 

  template<typename R> struct resistor : component<R>{
    R resistance;
    
    R get_imp(bool b){
      return resistance;
    }

    constexpr static auto name(){ return "resistor"; }
    
    static resistor<R> * init(R r){
      return new resistor<R>(r);
    }
    
    resistor(R r) : resistance(r){}
  };

  template<typename R> struct battery : component<R>{
    R voltage;
    
    R get_rise(){
      return voltage;
    }
    
    void set_IV(R I, R V){ return; }
    
    constexpr static auto name(){ return "battery"; }
    
    static battery<R> * init(R r){
      return new battery<R>(r);
    }
    
    battery(R v) : voltage(v) {}
  };

  template<typename R> struct inductor : component<R>{
    R inductance;
    R impedance = R(0);
    R I = R(0);
    R V = R(0);
    
    R get_imp(bool b){
      return impedance * (b ? R(1) : R(-1));
    }
    
    void set_IV(R I_, R V_){
      impedance = inductance * ((I_ - I) / dt) / I_;
		  if(isnan(impedance)) impedance = 0;
      I = I_, V = V_;
    }
    
    constexpr static auto name(){ return "inductor"; }
    
    static inductor<R> * init(R r){
      return new inductor<R>(r);
    }
    
    inductor(R L) : inductance(L) {}  
  };

  template<typename R> struct capacitor : component<R>{
    R capacitance;
    R impedance = R(0);
    R I = R(0);
    R V = R(0);
    
    R get_imp(bool b){
      return impedance * (b ? R(1) : R(-1));
    }
    
    void set_IV(R I_, R V_){
      impedance = V_ / (capacitance * ((V_ - V) / dt));
		  if(isnan(impedance)) impedance = 0;
      I = I_, V = V_;
    }
    
    constexpr static auto name(){ return "capacitor"; }
    
    static capacitor<R> * init(R r){
      return new capacitor<R>(r);
    }
    
    capacitor(R C) : capacitance(C) {}  
  };

  template<typename R, typename I, typename S, typename ... Fs>
  component<R> * find_match(I& in, S str, Fs ... fs){
    cout << "invalid_type_string :: " << str << endl;
    throw "invalid_type_string";
    return nullptr;
  }

  template<typename R, typename I, typename S, typename F, typename ... Fs>
  component<R> * find_match(I& in, S str, F f, Fs ... fs){
    if(get<0>(f) == str) return helper::send(in, get<1>(f));
    return find_match<R, I, S, Fs...>(in, str, fs...);
  }

  template<typename R> struct circuit{
	  vector<component<R>*> components;
	  vector<vector<int>> graph;

	  using cycle_t = vector<int>;
	  using vertex_t = vector<int>;

	  vector<tuple<R, R>> compute_IV(const vector<cycle_t>& cycles, const vector<vertex_t>& vertices){
		  mat::matrix<R> equations(components.size(), components.size() + 1);
		  auto advance = [this](size_t& in){ ++in; in = in % components.size(); };
		  size_t i(0);
		  for(cycle_t c : cycles){
		    size_t elem = 0;
		    for(int val : c){
		      if(val != 0){
            equations[i][elem] += components[elem] -> get_imp(val > 0);
            equations[i].back() += val * (components[elem] -> get_rise());
          }
          ++elem;
		    }
		    advance(i);
		  }
		  for(vertex_t v : vertices){
		      size_t elem = 0;
		      for(int val : v){
            if(val != 0) equations[i][elem] += ((val > 0) ? R(1) : R(-1));
            ++elem;
          }
          advance(i);		  
		  }
		  vector<R> result = mat::solve(equations);
		  vector<tuple<R, R>> to_return;
		  auto it = components.begin();
		  for(R val : result){
		    to_return.push_back(make_tuple(val, (*it) -> get_imp(true) * val));
		    (*it) -> set_IV(get<0>(to_return.back()), get<1>(to_return.back()));
		    ++it;
		  }
		  return to_return;
	  }

    vector<tuple<R, R>> compute_IV(){
      return compute_IV(get_cycles(), get_vertices());
    }

	  vector<cycle_t> get_cycles(){
		  auto expand = [this](vector<size_t> v){
			  cycle_t expanded(components.size(), 0);
			  for(size_t i(0); i < v.size() - 1; ++i){
				  expanded[abs(graph[v[i]][v[i + 1]]) - 1] = graph[v[i]][v[i + 1]] > 0 ? 1 : -1;
			  }
			  return expanded;
		  };
		  vector<vector<bool>> reduced_graph(graph.size(), vector<bool>(graph.size(), false));
		  for(size_t i(0); i < graph.size(); ++i){
			  for(size_t j(0); j < graph[i].size(); ++j){
				  reduced_graph[i][j] = graph[i][j] != 0;
			  }
		  }
		  vector<vector<size_t>> result(cyclical::cycles(reduced_graph.begin(), reduced_graph.end()));
		  vector<cycle_t> cycles{};
		  transform(result.begin(), result.end(), back_inserter(cycles), expand);
		  return cycles;
	  }

	  vector<vertex_t> get_vertices(){
		  auto expand = [this](const vector<int>& v){
			  vertex_t expanded(components.size(), 0);
			  for(int value : v){
				  if(value != 0){
					  expanded[abs(value) - 1] = (value > 0) ? 1 : -1;
				  }
			  }
			  return expanded;
		  };
		  vector<vertex_t> result{};
		  transform(graph.begin(), graph.end(), back_inserter(result), expand);
		  return result;
	  }

	  template<typename I, typename ... Fs> circuit(I& in, Fs ... fs){
	    size_t node_count(0); in >> node_count;
		  size_t comp_count(0); in >> comp_count;
		  string name;
		  for(size_t i(0); i < comp_count; ++i){
		    in >> name;
			  components.push_back(find_match<R, I, string, Fs...>(in, name, fs...));
		  }
		  graph = vector<vector<int>>(node_count, vector<int>(node_count, 0));
		  for(auto& row : graph){
		    for(int& elem : row){
		      in >> elem;
		    }
		  }
	  }

    circuit(circuit<R>&& input){
      graph.data() = input.graph.data();
      components.data() = input.data();
    }

    circuit<R>& operator=(circuit<R>&& input){
      graph.data() = input.graph.data();
      components.data() = input.data();
      return *this;
    }

    circuit<R>(circuit<R>& input) = delete;
    circuit<R>& operator=(circuit<R>& input) = delete;
    
	  ~circuit(){
		  for(component<R>* c : components){
		    if(c != nullptr) delete c;
		  }
	  }
  };

  template<typename R, typename ... Cs>
  circuit<R> make_circuit(auto& in){
    return circuit<R>(in, make_tuple(Cs::name(), helper::make_callable(Cs::init))...);
  }

  template<typename R, template<typename> typename ... Cs>
  circuit<R> make_circuit(auto& in){
    return circuit<R>(in, make_tuple(Cs<R>::name(), helper::make_callable(Cs<R>::init))...);
  }
}
