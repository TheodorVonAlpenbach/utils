#ifndef _DISJOINT_H_
#define _DISJOINT_H_

#include "point.h"
#include "segment.h"
#include "path.h"
#include "envelope.h"
#include "interval.h"
#include <functional>

namespace mb {
  template<class T, class P>
  struct Disjoint : std::unary_function<segment<P>, bool> {
    const point<T>& p_;
    Disjoint(const point<T>& p) : p_(p) {}
    bool operator()(const segment<P>& s) const {return disjoint(p_, s);}
  };

  template<class T>
  inline bool disjoint(const T x, const interval<T>& i) {
    return (x < i.min() - eps) || (i.max() + eps < x); }
  
  template<class T>
  inline bool disjoint(const interval<T>& x, const interval<T>& y) {
    return (x.max() + eps < y.min()) || (y.max() + eps < x.min()); }
  
  template<class T>
  inline bool disjoint(const point<T>& p, const envelope<T>& e) {
    return disjoint(p.x(), e.xI()) || disjoint(p.y(), e.yI()); }

  template<class T, class P>
  inline bool disjoint(const point<T>& p, const segment<P>& s) {
    return (disjoint(p, envelopeOf(s))) ||
      (!eq(dx(s) * (p.y() - s.front().y()), 
	   dy(s) * (p.x() - s.front().x())));}

} //mb

#endif //ifndef _DISJOINT_H_
