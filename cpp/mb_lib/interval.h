#ifndef _INTERVAL_H_
#define _INTERVAL_H_

#include <limits>

#ifdef _MSC_VER
#undef min
#undef max
#endif

namespace mb {
  template<class T>
  class interval { 
    // closed, ie. [a, b], default is (inf, -inf) or Ø    
  public:
    interval(const T min = std::numeric_limits<T>::max(),
	     const T max = std::numeric_limits<T>::min())
      : min_(min), max_(max) {}
    interval(const interval<T>& i) : min_(i.min()), max_(i.max()) {}
    T min() const {return min_;}
    T max() const {return max_;}
    T& min() {return min_;}
    T& max() {return max_;}
    bool contains(const T c) const {return min_ <= c && c <= max_;}
  private:
    T min_, max_;
  };

  typedef interval<double> intervalD;
  typedef interval<int> intervalI;
} //mb

template<class T> inline std::ostream& 
operator<< (std::ostream& os, const mb::interval<T>& i) {
  os << "[" << i.min() << ", " << i.max() << "]"; return os;
}

#endif //ifndef _INTERVAL_H_
