#include <iostream>
using namespace std;

struct Nil {};
template<class H, class T = Nil>
struct Cons {
    typedef H head;
    typedef T tail;
};

template<class L>
void print_list() {
    cout<< typeid(typename L::head).name() << " ";
    print_list<typename L::tail>();
}

template<>
void print_list<Nil>() {
    cout<< endl;
}

template<class H, typename ...Args> 
struct make_list {
    typedef Cons<H, typename make_list<Args...>::type> type;
};

template<class H> 
struct make_list<H> {
    typedef Cons<H> type;
};

template<class List>
struct Reverse {

    template<class R, class H> 
    struct go {
        typedef typename go< Cons<typename H::head, R>, typename H::tail>::type type;
    };
    template<class R>
    struct go<R, Nil> {
        typedef R type;
    };

    
    typedef typename go<Nil, List>::type type;
};

template<class T, class =int> 
struct test {
};

