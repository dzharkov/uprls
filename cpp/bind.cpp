#include<iostream>

using namespace std;

template<typename... Args>
struct Tuple {
};

template<typename Head, typename... Args>
struct Tuple<Head, Args...> {
    Head h;
    Tuple<Args...> tail;
};

Tuple<> Make_tuple() {
    return Tuple<>();
}

template<typename Head, typename... Args> 
Tuple<Head, Args...> Make_tuple(Head head, Args... args) {
    return Tuple<Head, Args...>{head, Make_tuple(args...)};
}

template<int N, typename Head, typename... Args>
struct getNthType {
    typedef typename getNthType<N-1, Args...>::type type;
};
template<typename Head, typename... Args>
struct getNthType<0, Head, Args...> {
    typedef Head type;
};

template<int N, typename Head, typename... Args>
struct getNthH {
    static typename getNthType<N, Head, Args...>::type call(Tuple<Head, Args...> tuple) {
        return getNthH<N-1, Args...>::call(tuple.tail);
    }
};

template<typename Head, typename... Args>
struct getNthH<0, Head, Args...> {
    static typename getNthType<0, Head, Args...>::type call(Tuple<Head, Args...> tuple) {
        return tuple.h;
    }
};

template<int N, typename... Args>
typename getNthType<N, Args...>::type getNth(Tuple<Args...> tuple) {
    return getNthH<N, Args...>::call(tuple);
}

template<int N> 
struct Ph {};

Ph<1> _1;
Ph<2> _2;
Ph<3> _3;
Ph<4> _4;
Ph<5> _5;

template<typename F, typename... CallArgs, int N, typename... Tail, typename ...Args>
void unwind_tuple_for_call(F f, Tuple<CallArgs...> call_args, Tuple<Ph<N>, Tail...> tuple, Args... args) {
    unwind_tuple_for_call(f, call_args, tuple.tail, args..., getNth<N-1, CallArgs...>(call_args));
}

template<typename F, typename... CallArgs, typename Head, typename... Tail, typename ...Args>
void unwind_tuple_for_call(F f, Tuple<CallArgs...> call_args, Tuple<Head, Tail...> tuple, Args... args) {
    unwind_tuple_for_call(f, call_args, tuple.tail, args..., tuple.h);
}

template<typename F, typename... CallArgs, typename ...Args>
void unwind_tuple_for_call(F f, Tuple<CallArgs...> call_args, Tuple<>, Args... args) {
    f(args...);
}

template<typename F, typename... Args>
struct BindH {
    BindH(F f, Args... args) 
        : f_(f), 
          args_(Make_tuple(args...))
          {}
    F f_;
    Tuple<Args...> args_;

    template<typename... CallArgs, typename... FArgs>
    void operator()(CallArgs... args) {
        return unwind_tuple_for_call(f_, Make_tuple(args...), args_);
    }
};

template<typename F, typename... CallArgs, typename... Args>
BindH<F, Args...> Bind(F f, Args... args) {
    return BindH<F, Args...>{f, args...};
}

void func(int a,int b, int c) {
    cout<< a << b << c << endl;
}

int main(int, char **){
    
    Bind(func, 1,2,3)();
    Bind(func, _1,_2,_1)(1,2);
    Bind(func, _3,_2,_1)(3,2,1);
    Bind(func, 1, 2, _1)(3);
    Bind(func, _1, 2, _2)(1,3);
    Bind(func, _2, _1, 3)(2,1);

    return 0;
}


