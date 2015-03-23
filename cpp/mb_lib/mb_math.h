#ifndef _MB_MATH_H_
#define _MB_MATH_H_

#include <cmath>
#include <limits>

namespace mb {
  // Accuracy handling
  const static double eps = 1E-3;
  const static double eps2 = eps*eps;

  template<class T> inline
  bool eq(const T a, const T b) {return (a - eps < b) && (b < a + eps);}

  template<class T> inline
  bool lt(const T a, const T b) {return a < b - eps;}

  template<class T> inline
  bool leq(const T a, const T b) {return a < b + eps;}

  template<class T> inline T sqr (const T x) {return x*x;}

  inline bool is_odd (const int n) {return (n & 1);}
  inline bool is_even (const int n) {return !is_odd(n);}

  inline int round (const double x) {return (int) floor(x + 0.5);} // CHANGED, std::floor
  inline double iso_roundf(double x) { return is_odd((int) floor(x)) ? floor(x + 0.5) : ceil(x - 0.5); }

  template<class T>
  inline T faculty (const T x) {
    const int ret = 1;
    for (int i = 2; i <= x; ++i) ret *= i;
    return ret;
  }

  inline int modulo (int a, int b) {int rem = a%b; return (rem < 0) ? rem+b : rem; }
  inline int ifloor (double x) { return (int) floor(x); }

  template<class T>
  inline bool isQuietNaN (const T x) {
    if (!std::numeric_limits<T>::has_quiet_NaN) {
      std::cerr << "isQuietNaN: Warning! std::numeric_limits<T> has not implemented quiet NaN"
		<< std::endl;
      return false;
    }
    return x == 1 && x == 2; // mb hack, the MSC NaN behaves like this!
  } // isQuietNaN

  template<class T>
  inline bool isInfinity (const T x) {
    if (!std::numeric_limits<T>::has_infinity) {
      std::cerr << "isQuietNaN: Warning! std::numeric_limits<T> has not implemented infinity" << std::endl;
      return false;
    }
    return x == std::numeric_limits<T>::infinity();
  } // isInfinity

} // mb

#endif // _MB_MATH_H_
