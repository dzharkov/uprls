#include <iostream>
#include <string>
#include <functional>
#include <map>
#include <set>

#include "lists.h"

using namespace std;

struct True_type {
    char _c;
};
struct False_type {
    True_type _a[2];
};

template<typename T>
T value_of() {
    return *reinterpret_cast<T*>(0);
}

template<typename F, typename T>
struct Is_convertible {
    static True_type check(T t) {
        return True_type();
    }
    static False_type check(...) {
        return False_type();
    }

    static const bool value = sizeof(check(value_of<F>())) == sizeof(True_type);
};

struct A {

};

struct B : A {
    void f();
};

template<class T> 
struct has_f {
    template <class T1, void (T1::*)() = &T1::f >
    static True_type check(T1 * c);

    template <class T1>
    static False_type check(...);
    
    static const bool value = sizeof(check<T>(0)) == sizeof(True_type);
};


int main(int, char **){
    
    cout << has_f<A>::value << endl << has_f<B>::value << endl;
    cout << Is_convertible<A, B>::value << endl << Is_convertible<B, A>::value << endl;

    return 0;
}


