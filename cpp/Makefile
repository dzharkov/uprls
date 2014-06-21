execs = main bind
all: $(execs) 
%: %.cpp
	g++ -pedantic -std=c++11 $< -o $@ 

.PHONY: clean

clean:
	rm -rf $(execs) 

