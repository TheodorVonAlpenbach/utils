#ifndef __MBE_STL_INTERNAL_FUNCTION_SPEC_H
#define __MBE_STL_INTERNAL_FUNCTION_SPEC_H

#include <mb_functional.h>

// Contains specializations of some function objects in std
// <functional>.

namespace mb {
  // Generators
  template<class T>
  struct sequence_gen : generator<T> {
    T i_;
    const T d_;
    sequence_gen(T i, const T d) : i_(i), d_(d) {}
    T operator()() {T ret = i_; i_ += d_; return ret;}
  };

  template<class T> 
  inline sequence_gen<T> sequenceGen(T a, const T d = 1)
  {return sequence_gen<T>(a, d);}

  // Unary functors

  // Specializations
  template <class _Arg> struct predicate : std::unary_function<_Arg, bool> {};

  template<class T>
  struct True : predicate<T> {
    bool operator() (const T& x) const {return true;}
  };

  template<class T, class Key = std::identity<T> >
  struct equalTo_t : predicate<T> {
    const T& x_;
    Key key_;
    explicit equalTo_t(const T& x, Key key) : x_(x), key_(key) {}
    explicit equalTo_t(const T& x) : x_(x) {}
    bool operator() (const T& y) const {return x_ == key_(y);}
  };

  template<class T, class Key> 
  equalTo_t<T, Key> equalTo(const T& x, Key key) { return equalTo_t<T, Key>(x, key); }

  template<class T> 
  equalTo_t<T> equalTo(const T& x) { return equalTo_t<T>(x); }
}

#endif //__MBE_STL_INTERNAL_FUNCTION_SPEC_H
