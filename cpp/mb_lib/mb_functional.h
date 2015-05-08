#ifndef __MBE_STL_INTERNAL_FUNCTION_H
#define __MBE_STL_INTERNAL_FUNCTION_H

#include <functional>

// Contains additions to std <functional>. These are rmem_fun_ref_t,
//  const_rmem_fun_ref_t, rmem_fun_ref.

// constant function object: always returns value of init argument.

namespace mb
{
  template <class _Result>
  struct generator {
    typedef _Result result_type;
  };

  template<class T, class A>
  class const1_t : public std::unary_function<T, T> {
  public:
    explicit const1_t(T t) : t_(t) {}
    T operator()(A) const {return t_;}
  private:
    T t_;
  };

  template<class T, class A>
  inline const1_t<T, A> const1(T t)
  {
    return const1_t<T, A>(t);
  }

  template <class UF1, class UF2>
  struct both_t
    : std::unary_function<typename UF1::argument_type, void>
  {
    UF1 f1_;
    UF2 f2_;
    both_t(UF1 f1, UF2 f2) : f1_(f1), f2_(f2) {}
    typename UF2::result_type
    operator()(const typename UF2::argument_type& x)
    {f1_(x); f2_(x);}
  };

  template <class UF1, class UF2>
  both_t<UF1, UF2> both(const UF1& f1, const UF2& f2)
  {
    return both_t<UF1, UF2>(f1, f2);
  };

  template <class UP1, class UP2>
  struct p_and_t
    : std::unary_function<typename UP2::argument_type, bool>
  {
    const UP1& p1_;
    const UP2& p2_;
    p_and_t(const UP1& p1, const UP2& p2) : p1_(p1), p2_(p2) {}
    bool operator()(const typename UP2::argument_type& x) const
    {return p1_(x) && p2_(x);}
  };

  template <class UP1, class UP2>
  p_and_t<UP1, UP2> p_and(const UP1& p1, const UP2& p2)
  {
    return p_and_t<UP1, UP2>(p1, p2);
  };

  // Adaptor function objects: pointers to member functions of objects
  //  not referenced by iterators. These are modelled from the std
  //  functor mem_fun family (i e mem_fun_t, mem_fun_ref_t etc). The
  //  difference is that the latter are functors act as member
  //  functions of the functor argument object, i e:
  //    vector<string> ss = ...;
  //    remove_if(ss.begin(), ss.end(), mem_fun_ref(&string::empty));
  //  while the former act as member function of a given object with the
  //  referenced iterator as argument:
  //    bool Date::isNotValid(const string& s) const {return ...;}
  //    remove_if(ss.begin(), ss.end(), rmem_fun_ref(&Date::isNotValid));

  // There are a total of 8 = 2^3 function objects in this family.
  //  (1) Call through pointer vs call through reference.
  //  (2) Member function with void return type vs member function with
  //      non-void return type.
  //  (3) Const vs non-const member function.

  // Note that choice (3) is not present in the 8/97 draft C++ standard, 
  //  which only allows these adaptors to be used with non-const functions.
  //  This is likely to be recified before the standard becomes final.
  // Note also that choice (2) is nothing more than a workaround: according
  //  to the draft, compilers should handle void and non-void the same way.
  //  This feature is not yet widely implemented, though.  You can only use
  //  member functions returning void if your compiler supports partial
  //  specialization.

  // All of this complexity is in the function objects themselves.  You can
  //  ignore it by using the helper function rmem_fun, rmem_fun_ref,
  //  mem_fun1, and mem_fun1_ref, which create whichever type of adaptor
  //  is appropriate.

  // (The preface is adopted from SGI's <std_function.h>) 1996)


  template<class T, class A, class R>
  class rmem_fun_ref_t : public std::unary_function<A, R>
  {
  public:
    explicit rmem_fun_ref_t(T* t, R (T::*p)(A))
      : t_(t), p_(p) {}
    R operator()(A a) {return (t_->*p_)(a);}
  private:
    T* t_;
    R (T::*p_)(A);
  };

  template<class T, class A, class R>
  class const_rmem_fun_ref_t : public std::unary_function<A, R>
  {
  public:
    explicit const_rmem_fun_ref_t(const T* t, R (T::*p)(A) const)
      : t_(t), p_(p) {}
    R operator()(A a) const {return (t_->*p_)(a);}
  private:
    const T* t_;
    R (T::*p_)(A) const;
  };

#ifndef _MSC_VER
  
  template<class T, class A, class R>
  class const_rmem_fun_ref_t<T, const A&, R>
    : public std::unary_function<A, R>
  {
  public:
    explicit const_rmem_fun_ref_t(const T* t, R (T::*p)(const A&) const)
      : t_(t), p_(p) {}
    R operator()(const A& a) const {return (t_->*p_)(a);}
  private:
    const T* t_;
    R (T::*p_)(const A&) const;
  };


  template<class T, class A>
  class rmem_fun_ref_t<T, A, void>
    : public std::unary_function<A, void>
  {
  public:
    explicit rmem_fun_ref_t(T* t, void (T::*p)(A))
      : t_(t), p_(p) {}
    void operator()(A a) {(t_->*p_)(a);}
  private:
    T* t_;
    void (T::*p_)(A);
  };

  template<class T, class A>
  class const_rmem_fun_ref_t<T, A, void>
    : public std::unary_function<A, void>
  {
  public:
    explicit const_rmem_fun_ref_t(const T* t, void (T::*p)(A) const)
      : t_(t), p_(p) {}
    void operator()(A a) const {(t_->*p_)(a);}
  private:
    const T* t_;
    void (T::*p_)(A) const;
  };

  template<class T, class A>
  class const_rmem_fun_ref_t<T, const A&, void>
    : public std::unary_function<A, void>
  {
  public:
    explicit const_rmem_fun_ref_t(const T* t, void (T::*p)(const A&) const)
      : t_(t), p_(p) {}
    void operator()(const A& a) const {(t_->*p_)(a);}
  private:
    const T* t_;
    void (T::*p_)(const A&) const;
  };

#endif // #ifndef _MSC_VER

  template<class T, class A, class R>
  inline const_rmem_fun_ref_t<T, A, R>
  rmem_fun_ref(const T* t, R (T::*f)(A) const)
  {
    return const_rmem_fun_ref_t<T, A, R>(t, f);
  }

  template<class T, class A, class R>
  inline const_rmem_fun_ref_t<T, A, R>
  rmem_fun_ref(T* t, R (T::*f)(A) const)
  {
    return const_rmem_fun_ref_t<T, A, R>(const_cast<const T*>(t), f);
  }

  template<class T, class A, class R>
  inline rmem_fun_ref_t<T, A, R>
  rmem_fun_ref(T* t, R (T::*f)(A))
  {
    return rmem_fun_ref_t<T, A, R>(t, f);
  }

  template <class Pair>
  struct init2nd : public std::unary_function<typename Pair::second_type, Pair>
  {
    const typename Pair::first_type& first_;
    init2nd(const typename Pair::first_type& first) : first_(first) {} 
    Pair operator()(const typename Pair::second_type& second) const
    {return Pair(first_, second);}
  };
} // mb

namespace std {
 #if !defined(__SGI_STL_INTERNAL_FUNCTION_H) && !defined(__unix__)
  // identity is an extensions: it is not part of the standard.
  template <class _Tp>
  struct _Identity : public std::unary_function<_Tp,_Tp> {
    const _Tp& operator()(const _Tp& __x) const { return __x; }
  };

  // select1st and select2nd are extensions: they are not part of the standard.
  template <class _Pair>
  struct _Select1st : public std::unary_function<_Pair, typename _Pair::first_type> {
    const typename _Pair::first_type& operator()(const _Pair& __x) const {
      return __x.first;
    }
  };

  template <class _Pair>
  struct _Select2nd : public std::unary_function<_Pair, typename _Pair::second_type>
  {
    const typename _Pair::second_type& operator()(const _Pair& __x) const {
      return __x.second;
    }
  };

  template <class _Pair> struct select1st : public _Select1st<_Pair> {};
  template <class _Pair> struct select2nd : public _Select2nd<_Pair> {};

#endif // __SGI_STL_INTERNAL_FUNCTION_H

#if !defined(__SGI_STL_INTERNAL_FUNCTION_H)
  template <class _Tp> struct identity : public _Identity<_Tp> {};
#endif

}

// std functors (used as examples)

// template <class S, class T>
// class mem_fun_t : public std::unary_function<T*, S> {
// public:
//   explicit mem_fun_t(S (T::*pf)()) : f(pf) {}
//   S operator()(T* p) const { return (p->*f)(); }
// private:
//   S (T::*f)();
// };

// template <class S, class T>
// class const_mem_fun_t : public std::unary_function<const T*, S> {
// public:
//   explicit const_mem_fun_t(S (T::*pf)() const) : f(pf) {}
//   S operator()(const T* p) const { return (p->*f)(); }
// private:
//   S (T::*f)() const;
// };


// template <class S, class T>
// class mem_fun_ref_t : public std::unary_function<T, S> {
// public:
//   explicit mem_fun_ref_t(S (T::*pf)()) : f(pf) {}
//   S operator()(T& r) const { return (r.*f)(); }
// private:
//   S (T::*f)();
// };

// template <class S, class T>
// class const_mem_fun_ref_t : public std::unary_function<T, S> {
// public:
//   explicit const_mem_fun_ref_t(S (T::*pf)() const) : f(pf) {}
//   S operator()(const T& r) const { return (r.*f)(); }
// private:
//   S (T::*f)() const;
// };

// template <class S, class T, class A>
// class mem_fun1_t : public std::binary_function<T*, A, S> {
// public:
//   explicit mem_fun1_t(S (T::*pf)(A)) : f(pf) {}
//   S operator()(T* p, A x) const { return (p->*f)(x); }
// private:
//   S (T::*f)(A);
// };

// template <class S, class T, class A>
// class const_mem_fun1_t : public std::binary_function<const T*, A, S> {
// public:
//   explicit const_mem_fun1_t(S (T::*pf)(A) const) : f(pf) {}
//   S operator()(const T* p, A x) const { return (p->*f)(x); }
// private:
//   S (T::*f)(A) const;
// };

// template <class S, class T, class A>
// class mem_fun1_ref_t : public std::binary_function<T, A, S> {
// public:
//   explicit mem_fun1_ref_t(S (T::*pf)(A)) : f(pf) {}
//   S operator()(T& r, A x) const { return (r.*f)(x); }
// private:
//   S (T::*f)(A);
// };

// template <class S, class T, class A>
// class const_mem_fun1_ref_t : public std::binary_function<T, A, S> {
// public:
//   explicit const_mem_fun1_ref_t(S (T::*pf)(A) const) : f(pf) {}
//   S operator()(const T& r, A x) const { return (r.*f)(x); }
// private:
//   S (T::*f)(A) const;
// };

// #ifdef __STL_CLASS_PARTIAL_SPECIALIZATION

// template <class T>
// class mem_fun_t<void, T> : public std::unary_function<T*, void> {
// public:
//   explicit mem_fun_t(void (T::*pf)()) : f(pf) {}
//   void operator()(T* p) const { (p->*f)(); }
// private:
//   void (T::*f)();
// };

// template <class T>
// class const_mem_fun_t<void, T> : public std::unary_function<const T*, void> {
// public:
//   explicit const_mem_fun_t(void (T::*pf)() const) : f(pf) {}
//   void operator()(const T* p) const { (p->*f)(); }
// private:
//   void (T::*f)() const;
// };

// template <class T>
// class mem_fun_ref_t<void, T> : public std::unary_function<T, void> {
// public:
//   explicit mem_fun_ref_t(void (T::*pf)()) : f(pf) {}
//   void operator()(T& r) const { (r.*f)(); }
// private:
//   void (T::*f)();
// };

// template <class T>
// class const_mem_fun_ref_t<void, T> : public std::unary_function<T, void> {
// public:
//   explicit const_mem_fun_ref_t(void (T::*pf)() const) : f(pf) {}
//   void operator()(const T& r) const { (r.*f)(); }
// private:
//   void (T::*f)() const;
// };

// template <class T, class A>
// class mem_fun1_t<void, T, A> : public std::binary_function<T*, A, void> {
// public:
//   explicit mem_fun1_t(void (T::*pf)(A)) : f(pf) {}
//   void operator()(T* p, A x) const { (p->*f)(x); }
// private:
//   void (T::*f)(A);
// };

// template <class T, class A>
// class const_mem_fun1_t<void, T, A> : public std::binary_function<const T*, A, void> {
// public:
//   explicit const_mem_fun1_t(void (T::*pf)(A) const) : f(pf) {}
//   void operator()(const T* p, A x) const { (p->*f)(x); }
// private:
//   void (T::*f)(A) const;
// };

// template <class T, class A>
// class mem_fun1_ref_t<void, T, A> : public std::binary_function<T, A, void> {
// public:
//   explicit mem_fun1_ref_t(void (T::*pf)(A)) : f(pf) {}
//   void operator()(T& r, A x) const { (r.*f)(x); }
// private:
//   void (T::*f)(A);
// };

// template <class T, class A>
// class const_mem_fun1_ref_t<void, T, A> : public std::binary_function<T, A, void> {
// public:
//   explicit const_mem_fun1_ref_t(void (T::*pf)(A) const) : f(pf) {}
//   void operator()(const T& r, A x) const { (r.*f)(x); }
// private:
//   void (T::*f)(A) const;
// };

// #endif /* __STL_CLASS_PARTIAL_SPECIALIZATION */

// // Mem_fun adaptor helper functions.  There are only four:
// //  mem_fun, mem_fun_ref, mem_fun1, mem_fun1_ref.

// template <class S, class T>
// inline mem_fun_t<S,T> mem_fun(S (T::*f)()) { 
//   return mem_fun_t<S,T>(f);
// }

// template <class S, class T>
// inline const_mem_fun_t<S,T> mem_fun(S (T::*f)() const) {
//   return const_mem_fun_t<S,T>(f);
// }

// template <class S, class T>
// inline mem_fun_ref_t<S,T> mem_fun_ref(S (T::*f)()) { 
//   return mem_fun_ref_t<S,T>(f);
// }

// template <class S, class T>
// inline const_mem_fun_ref_t<S,T> mem_fun_ref(S (T::*f)() const) {
//   return const_mem_fun_ref_t<S,T>(f);
// }

// template <class S, class T, class A>
// inline mem_fun1_t<S,T,A> mem_fun1(S (T::*f)(A)) { 
//   return mem_fun1_t<S,T,A>(f);
// }

// template <class S, class T, class A>
// inline const_mem_fun1_t<S,T,A> mem_fun1(S (T::*f)(A) const) {
//   return const_mem_fun1_t<S,T,A>(f);
// }

// template <class S, class T, class A>
// inline mem_fun1_ref_t<S,T,A> mem_fun1_ref(S (T::*f)(A)) { 
//   return mem_fun1_ref_t<S,T,A>(f);
// }

// template <class S, class T, class A>
// inline const_mem_fun1_ref_t<S,T,A> mem_fun1_ref(S (T::*f)(A) const) {
//   return const_mem_fun1_ref_t<S,T,A>(f);
// }

#endif //__MBE_STL_INTERNAL_FUNCTION_H
